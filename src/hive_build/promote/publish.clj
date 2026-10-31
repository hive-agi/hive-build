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
   :target/private? false
   :target/repo-url "https://repo.clojars.org"
   :target/repo-url-env nil
   :target/repository-name nil
   :target/username-env "CLOJARS_USERNAME"
   :target/password-env "CLOJARS_PASSWORD"})

(def gitea
  {:target/id :gitea
   :target/artifact-kind :artifact/aot
   :target/publishes? true
   ;; Readable only to credential holders, so the opacity gate defaults to
   ;; strict here: see `private?`.
   :target/private? true
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

(def clojars-aot
  "Public Clojars, AOT jar: the interfaces of a library's protocols ship as
   class files, so AOT consumers load from a plain classpath."
  (assoc clojars
         :target/id :clojars-aot
         :target/artifact-kind :artifact/aot))

(def none
  {:target/id :none
   :target/artifact-kind :artifact/source
   :target/publishes? false
   :target/private? false
   :target/repo-url nil
   :target/repo-url-env nil
   :target/repository-name nil
   :target/username-env nil
   :target/password-env nil})

(def default-targets
  [clojars clojars-aot gitea gitea-source none])

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

(defn private?
  "True when `id` names a destination whose artifacts only credential holders
   can read.

   An id nobody registered answers TRUE: this is the default source for the
   opacity gate's strictness, and an unregistered destination must not be the
   reason a gate is skipped. Such an id still stops the release at `target`."
  [id]
  (if-let [t (get @registry id)]
    (boolean (:target/private? t))
    true))

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

(defn registry-state
  "What the registry holds, given what a HEAD of the pom and of the jar
   answered. Each argument is :present, :absent or :unreachable.

   A coordinate is both documents. Reading only one of them cannot tell a
   spent version from a half-landed one, and under Maven immutability a
   half-landed one can never be completed: it has to be deleted."
  [pom jar]
  (cond
    (or (= :unreachable pom) (= :unreachable jar)) :unknown
    (= :present pom jar)                           :complete
    (= :absent pom jar)                            :absent
    :else                                          :partial))

(def ProbeState
  "What a post-deploy probe found: a RegistryState, or the reason no probe
   could be made (:no-url, :no-credentials)."
  [:enum :absent :complete :partial :unknown :no-url :no-credentials])

(def VerifyInput
  [:map {:closed true}
   [:verify/label [:string {:min 1}]]
   [:verify/state ProbeState]])

(def Verdict
  [:map {:closed true}
   [:verdict/ok? :boolean]
   [:verdict/state ProbeState]
   [:verdict/message [:string {:min 1}]]])

(defn probe-auth
  "How a post-deploy probe of `target` must be made with `env`:
   {:probe/url u :probe/auth a}, or {:probe/skip reason} when it cannot be.

   A private target with no credentials is NOT probed: its registry answers 401
   to an anonymous read, which is indistinguishable from `missing` and would
   send the operator hunting for a deploy bug that is a secret problem."
  [target env]
  (let [url (repo-url target env)
        auth (basic-auth (get env (:target/username-env target))
                         (get env (:target/password-env target)))]
    (cond
      (nil? url) {:probe/skip :no-url}
      (and (:target/private? target) (nil? auth)) {:probe/skip :no-credentials}
      :else {:probe/url url :probe/auth auth})))

(defn verdict
  "Whether a release run may report success, given what the registry holds
   for the coordinate it claims to have published. Only :complete passes:
   every other answer, including `could not look`, fails the run."
  [{:verify/keys [label state]}]
  {:verdict/ok? (= :complete state)
   :verdict/state state
   :verdict/message
   (case state
     :complete (str label " is in the registry (pom and jar).")
     :absent (str label " is NOT in the registry: the release published nothing.")
     :partial (str label " is half-published: one of pom/jar is missing."
                   " Delete the version from the registry and re-release.")
     :unknown (str label " could not be verified: the registry was unreachable.")
     :no-url (str label " could not be verified: the target's repository URL"
                  " variable is unset, so nothing can have been published either.")
     :no-credentials (str label " could not be verified: the registry credentials"
                          " are unset, so the deploy step had none either."))})

(m/=> probe-auth [:=> [:cat s/Target [:map-of :string :string]] :map])
(m/=> verdict [:=> [:cat VerifyInput] Verdict])

(m/=> register! [:=> [:cat s/Target] :keyword])
(m/=> target [:=> [:cat :keyword] s/Target])

(m/=> private? [:=> [:cat :keyword] :boolean])
(m/=> required-env [:=> [:cat s/Target] [:vector [:string {:min 1}]]])
(m/=> repo-url [:=> [:cat s/Target [:map-of :string :string]] [:maybe :string]])
(m/=> deploy-request [:=> [:cat s/Target :map] :map])
