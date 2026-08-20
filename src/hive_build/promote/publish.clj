(ns hive-build.promote.publish
  "Publish destinations as data, behind a registry. Pure.

   The registry is the dependency-inversion point: `plan` and the boundary
   depend on the Target schema, never on a particular registry. A new
   destination is a `register!` and no edit anywhere else."
  (:require [malli.core :as m]
            [hive-build.schema :as s]))

(def clojars
  {:target/id :clojars
   :target/artifact-kind :artifact/source
   :target/publishes? true
   :target/repo-url "https://repo.clojars.org"
   :target/repo-url-env nil
   :target/repository-name nil
   :target/username-env "CLOJARS_USERNAME"
   :target/password-env "CLOJARS_PASSWORD"})

(def gitea
  {:target/id :gitea
   :target/artifact-kind :artifact/aot
   :target/publishes? true
   :target/repo-url nil
   :target/repo-url-env "MAVEN_URL"
   ;; The id this repository is known by in a project's :mvn/repos AND in
   ;; ~/.m2/settings.xml. deps-deploy resolves an id against both, so the three
   ;; must agree or a credential-free deploy cannot find its URL.
   :target/repository-name "hive-gitea"
   :target/username-env "MAVEN_USERNAME"
   :target/password-env "MAVEN_TOKEN"})

(def gitea-source
  (assoc gitea
         :target/id :gitea-source
         :target/artifact-kind :artifact/source))

(def none
  {:target/id :none
   :target/artifact-kind :artifact/source
   :target/publishes? false
   :target/repo-url nil
   :target/repo-url-env nil
   :target/repository-name nil
   :target/username-env nil
   :target/password-env nil})

(def default-targets
  [clojars gitea gitea-source none])

(defonce ^:private registry (atom {}))

(defn register!
  "Add `target` to the registry, replacing any target with the same id.
   Returns the id."
  [target]
  (swap! registry assoc (:target/id target) target)
  (:target/id target))

(defn deregister!
  [id]
  (swap! registry dissoc id)
  id)

(defn target-ids
  []
  (set (keys @registry)))

(defn target
  "The registered Target for `id`.

   Throws when `id` is unknown: an unrecognised :publish value must stop the
   release, never fall through to a default destination."
  [id]
  (or (get @registry id)
      (throw (ex-info "version.edn :publish names no registered target"
                      {:publish id :registered (sort (target-ids))}))))

(run! register! default-targets)

(defn required-env
  "The environment variables `target` cannot publish without, in a stable
   order. Derived from the target, so a new destination declares its own
   credentials rather than editing a check."
  [target]
  (into [] (remove nil?) [(:target/repo-url-env target)
                          (:target/username-env target)
                          (:target/password-env target)]))

(defn repo-url
  "The Maven repository URL for `target`, reading `env` when the target names
   its URL by environment variable. nil when neither is available."
  [target env]
  (or (:target/repo-url target)
      (when-let [k (:target/repo-url-env target)] (get env k))))

(defn basic-auth
  "The Authorization header value for `username`/`password`, or nil when either
   is missing. Credentials are transmitted, not stored: this is the only place
   they are encoded."
  [username password]
  (when (and (seq username) (seq password))
    (str "Basic "
         (.encodeToString (java.util.Base64/getEncoder)
                          (.getBytes (str username ":" password) "UTF-8")))))

(defn deploy-request
  "The deps-deploy argument map for `target`.

   A named repository is passed as a MAP when `env` carries its credentials —
   that is how CI supplies them — and as the repository ID alone when it does
   not. deps-deploy resolves an id against the project's `:mvn/repos` for the
   URL and against ~/.m2/settings.xml for the username and password, which is
   where an operator's credentials already live and the only path that keeps a
   token out of the process environment. Without a named repository
   deps-deploy applies its own Clojars defaults, and a local install never
   carries a repository at all."
  [target {:keys [artifact pom-file env installer] :or {installer :remote}}]
  (let [repo-name (:target/repository-name target)
        username (get env (:target/username-env target))
        password (get env (:target/password-env target))
        named? (and repo-name (= :remote installer))]
    (cond-> {:installer installer
             :artifact artifact
             :pom-file pom-file}
      (and named? (seq username) (seq password))
      (assoc :repository
             {repo-name {:url (repo-url target env)
                         :username username
                         :password password}})

      (and named? (not (and (seq username) (seq password))))
      (assoc :repository repo-name))))

(m/=> register! [:=> [:cat s/Target] :keyword])
(m/=> target [:=> [:cat :keyword] s/Target])
(m/=> required-env [:=> [:cat s/Target] [:vector [:string {:min 1}]]])
(m/=> repo-url [:=> [:cat s/Target [:map-of :string :string]] [:maybe :string]])
(m/=> deploy-request [:=> [:cat s/Target :map] :map])
