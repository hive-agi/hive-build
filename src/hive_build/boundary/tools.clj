(ns hive-build.boundary.tools
  "The tools.build adapter: gathers the Project and Facts a plan is computed
   from, and supplies the handler that executes each step kind.

   This is the only namespace that knows tools.build and deps-deploy exist."
  (:require [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [hive-build.boundary.archive :as archive]
            [hive-build.boundary.load-verify :as load-verify]
            [hive-build.collect.io :as io']
            [hive-build.promote.manifest :as manifest]
            [hive-build.promote.naming :as naming]
            [hive-build.promote.pom :as pom]
            [hive-build.promote.project :as project]
            [hive-build.promote.publish :as publish]))

;; ── Context ────────────────────────────────────────────────────────────────

(defn resolve-version
  "The version this repository releases: ./VERSION when present, otherwise
   0.{:minor}.{commit count}."
  [cfg]
  (or (some-> (io'/read-text "VERSION") str/trim not-empty)
      (str "0." (:minor cfg 0) "." (b/git-count-revs nil))))

(defn read-project
  "The Project for the repository in the current working directory.

   Read at task time, never at require time: as a library, hive-build is
   loaded long before anyone chooses which repository to build."
  []
  (let [cfg (or (io'/read-edn "version.edn")
                (throw (ex-info "no ./version.edn in the current directory"
                                {:cwd (System/getProperty "user.dir")})))]
    (project/project cfg (resolve-version cfg))))

(defn overlay
  "Parsed ./local.deps.edn, or nil when absent. Supplies a :provided alias of
   host sources that must be on the AOT compile classpath but must not enter
   the pom, and an :aot/preload namespace vector compiled first."
  []
  (io'/read-edn "local.deps.edn"))

(defn published?
  "True when this exact coordinate already exists at `target`'s registry.

   Any failure to reach the registry is false: an unreachable registry must
   not be read as `already published`, because that would silently skip a
   release."
  [project target]
  (let [env-keys (publish/required-env target)
        env (try (io'/env env-keys) (catch Throwable _ nil))]
    (if (or (nil? env) (not (:target/publishes? target)))
      false
      (when-let [url (publish/repo-url target env)]
        (io'/head-ok? (naming/pom-url url (:project/coordinate project))
                      (publish/basic-auth (get env (:target/username-env target))
                                          (get env (:target/password-env target))))))))

(defn read-facts
  "Everything from the filesystem and the registry that `project`'s plan
   depends on.

   `probe-registry?` is false for tasks that never publish, so a local build
   never touches the network. `overlay` is the parsed ./local.deps.edn, or nil
   for a task that does not compile."
  [project {:keys [probe-registry? overlay]}]
  (let [src-dirs (:project/src-dirs project)
        roots (project/classify-roots src-dirs (io'/files-by-dir src-dirs))
        sources (into [] (comp (mapcat io'/files-under)
                               (filter project/source-file?)
                               (remove #(str/ends-with? % ".cljs")))
                      (:facts/source-roots roots))]
    (assoc roots
           :facts/namespaces (into [] (keep io'/declared-ns) sources)
           :facts/preload (vec (:aot/preload overlay))
           :facts/published? (boolean
                              (when probe-registry?
                                (published? project
                                            (publish/target (:project/target-id project))))))))

(defn read-license-facts
  "What the licence rules are evaluated against: what version.edn declares,
   whether ./LICENSE exists, and the SPDX identifiers the sources carry."
  [project]
  (let [sources (into [] (comp (mapcat io'/files-under)
                               (filter project/source-file?))
                      (:project/src-dirs project))]
    {:license.facts/declared (get-in project [:project/license :license/name])
     :license.facts/file? (io'/exists? "LICENSE")
     :license.facts/spdx-ids (io'/spdx-ids sources)}))

(defn probe-process
  "Run `command-args`, capturing both streams. For availability checks, and for
   commands whose stdout IS the answer."
  [command-args]
  (b/process {:command-args command-args :out :capture :err :capture}))

(defn run-process
  "Run `command-args` with its output going to the console.

   A linter whose findings were captured and discarded is a linter that always
   passes, so anything whose output is meant for a human is run this way."
  [command-args]
  (b/process {:command-args command-args}))

(defn kondo-available?
  []
  (io'/on-path? "clj-kondo" ["--version"] probe-process))

(defn deps-classpath
  "The classpath of this project under `aliases`."
  [aliases]
  (str/join java.io.File/pathSeparator
            (:classpath-roots (b/create-basis {:project "deps.edn"
                                               :user :standard
                                               :aliases aliases}))))

(defn bb-classpath
  "Resolve bb.edn deps so Babashka macro exporters also reach clj-kondo.
   nil when there is no bb.edn or bb is not installed."
  []
  (when (io'/exists? "bb.edn")
    (if-not (io'/on-path? "bb" ["--version"] probe-process)
      (do (println "Skip bb.edn dependency configs: bb not on PATH.") nil)
      (let [{:keys [exit out]} (probe-process ["bb" "print-deps" "--format" "classpath"])]
        (when (pos? exit)
          (throw (ex-info "Could not resolve bb.edn dependency classpath" {:exit exit})))
        (not-empty (str/trim out))))))

;; ── Bases ──────────────────────────────────────────────────────────────────

(defn pom-basis
  "Project basis for the pom, minus :pom-exclude-deps. Equals the plain basis
   when the key is absent."
  [project]
  (let [exclude (:project/pom-exclude-deps project)]
    (if (empty? exclude)
      (b/create-basis {:project "deps.edn" :user :standard})
      (let [proj (io'/read-edn "deps.edn")]
        (b/create-basis {:project (assoc proj :deps (pom/prune-deps (:deps proj) exclude))
                         :user :standard})))))

(defn aot-basis
  "Compile-time basis. Injects ONLY the overlay's :provided alias, so the
   overlay's top-level :deps never reach the release compile classpath."
  [overlay]
  (b/create-basis
   (cond-> {:project "deps.edn" :user :standard}
     (get-in overlay [:aliases :provided])
     (assoc :extra (update (select-keys overlay [:aliases]) :aliases select-keys [:provided])
            :aliases [:provided]))))

;; ── Handlers ───────────────────────────────────────────────────────────────

(defn- lib-of [ctx]
  (naming/lib-symbol (get-in ctx [:ctx/project :project/coordinate])))

(defn- pom-file [ctx]
  (b/pom-path {:lib (lib-of ctx)
               :class-dir (get-in ctx [:ctx/project :project/class-dir])}))

(defn- default-deploy!
  [request]
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy) request))

(def handlers
  "Step kind -> (fn [ctx step] -> result). The tools.build implementation of
   every step a plan can contain."
  {:step/clean
   (fn [_ctx step] (b/delete {:path (:step/path step)}))

   :step/compile
   (fn [ctx step]
     ;; :compile-opts, NOT :compiler-options — tools.build ignores unknown keys
     ;; silently, so the wrong spelling binds *compiler-options* to nil and
     ;; elides nothing while every build still reports success.
     (b/compile-clj
      (cond-> {:basis (aot-basis (:ctx/overlay ctx))
               :src-dirs (:step/src-dirs step)
               :ns-compile (:step/ns-compile step)
               :class-dir (:step/class-dir step)}
        (seq (:step/elide-meta step))
        (assoc :compile-opts {:elide-meta (:step/elide-meta step)})

        (seq (:step/java-opts step))
        (assoc :java-opts (:step/java-opts step)))))

   :step/copy-classes
   (fn [_ctx step]
     (archive/copy-own-classes! (:step/from step)
                                (:step/to step)
                                (:step/prefixes step)
                                (:step/files step)))

   :step/copy-dir
   (fn [_ctx step]
     (b/copy-dir {:src-dirs (:step/src-dirs step) :target-dir (:step/target-dir step)}))

   :step/stamp-manifest
   (fn [_ctx step]
     (let [version (:step/version step)]
       (into []
             (keep (fn [path]
                     (when-let [stamped (manifest/stamp (io'/read-text path) version)]
                       (io'/write-text! path stamped))))
             (manifest/manifests (io'/files-under (:step/class-dir step))))))

   :step/write-pom
   (fn [ctx _step]
     (let [{:ctx/keys [project]} ctx]
       (b/write-pom {:class-dir (:project/class-dir project)
                     :lib (lib-of ctx)
                     :version (get-in project [:project/coordinate :coordinate/version])
                     :basis (pom-basis project)
                     :src-dirs (pom/pom-src-dirs (:project/src-dirs project))
                     :scm (pom/scm (:project/scm-url project)
                                   (b/git-process {:git-args "rev-parse HEAD"}))
                     :pom-data (pom/pom-data (:project/license project))})))

   :step/jar
   (fn [_ctx step]
     (b/jar {:class-dir (:step/class-dir step) :jar-file (:step/jar-file step)}))

   :step/normalize
   (fn [_ctx step] (archive/normalize-jar! (:step/path step)))

   :step/verify-load
   (fn [ctx step]
     (load-verify/verify!
      {:basis (pom-basis (:ctx/project ctx))
       :jar-file (:step/jar-file step)
       :namespaces (:step/namespaces step)
       :java-opts (:step/java-opts step)}))

   :step/publish
   (fn [ctx step]
     (let [{:ctx/keys [project deploy-fn]} ctx
           target (publish/target (:step/target-id step))
           remote? (= :remote (:step/installer step))
           env (if remote? (io'/env (publish/required-env target)) {})]
       ((or deploy-fn default-deploy!)
        (publish/deploy-request target
                                {:artifact (:project/jar-file project)
                                 :pom-file (pom-file ctx)
                                 :env env
                                 :installer (:step/installer step)}))))

   :step/announce
   (fn [_ctx step] (println (:step/message step)))})

(defn context
  "The execution context for `project`: the overlay an AOT compile needs (nil
   for tasks that do not compile), and the deploy function, which is injected
   so a release can be exercised without a registry."
  ([project] (context project nil nil))
  ([project overlay] (context project overlay nil))
  ([project overlay deploy-fn]
   {:ctx/project project
    :ctx/overlay overlay
    :ctx/deploy-fn deploy-fn}))
