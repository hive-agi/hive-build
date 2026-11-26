(ns hive-build.pipeline.plan
  "Task + Project + Facts -> Plan. Pure and total.

   A release is a value before it is an effect. Everything read from the
   filesystem or the network already sits in `facts`, so the whole decision —
   which jar, whether to skip, what gets published where — can be asserted
   without building anything.

   Adding a task is a new `steps` method; adding a destination is a
   `publish/register!`. Neither edits an existing branch."
  (:require [malli.core :as m]
            [hive-build.promote.naming :as naming]
            [hive-build.promote.publish :as publish]
            [hive-build.schema :as s]))

(defn- announce [message]
  {:step/kind :step/announce :step/message message})

(defn- label [project]
  (naming/coordinate-label (:project/coordinate project)))

(defn- released [project]
  (get-in project [:project/coordinate :coordinate/version]))

(defmulti steps
  "Steps for one task. Dispatches on the task keyword."
  (fn [task _project _facts] task))

(defmethod steps :default
  [task _project _facts]
  (throw (ex-info "no plan for task" {:task task})))

(defmethod steps :task/clean
  [_ project _facts]
  [{:step/kind :step/clean :step/path (:project/target-dir project)}])

(defn- exclude-step
  "The prune of `:project/jar-excludes` from `class-dir`, or nil when the
   project excludes nothing."
  [project class-dir]
  (when-let [paths (seq (:project/jar-excludes project))]
    {:step/kind :step/exclude :step/class-dir class-dir :step/paths (vec paths)}))

(defn- verify-excluded-step
  "The read-back of the built jar against `:project/jar-excludes`, or nil
   when the project excludes nothing."
  [project jar-file]
  (when-let [paths (seq (:project/jar-excludes project))]
    {:step/kind :step/verify-excluded :step/jar-file jar-file :step/paths (vec paths)}))

(defn- verify-packaged-step
  "The check that no deps.edn :paths root holding files is left out of the
   jar, or nil when every root is packaged. Warns; `strict-packaging` makes a
   deploy refuse."
  [facts]
  (when-let [roots (seq (:facts/unpackaged-roots facts))]
    {:step/kind :step/verify-packaged :step/roots (vec roots) :step/strict? false}))

(defn- strict-packaging
  [step]
  (cond-> step
    (= :step/verify-packaged (:step/kind step)) (assoc :step/strict? true)))

(defmethod steps :task/jar
  [_ project facts]
  (let [{:project/keys [target-dir src-dirs class-dir jar-file]} project]
    (into []
          (remove nil?)
          [(verify-packaged-step facts)
           {:step/kind :step/clean :step/path target-dir}
           {:step/kind :step/write-pom}
           {:step/kind :step/copy-dir :step/src-dirs src-dirs :step/target-dir class-dir}
           (exclude-step project class-dir)
           {:step/kind :step/stamp-manifest :step/class-dir class-dir
            :step/version (released project)}
           {:step/kind :step/jar :step/class-dir class-dir :step/jar-file jar-file}
           {:step/kind :step/normalize :step/path jar-file}
           (verify-excluded-step project jar-file)
           (announce (str "Built " (label project) " " (released project) " -> " jar-file))])))

(defn source-namespace-files
  "Jar entry paths of `source-namespaces`, from `ns-files` when the facts know
   the file, else the .clj path the namespace name implies (so a missing one
   is reported by the copy as declared-but-absent rather than dropped)."
  [source-namespaces ns-files]
  (mapv #(get ns-files % (str (naming/ns->path %) ".clj")) source-namespaces))

(defmethod steps :task/jar-aot
  [_ project facts]
  (let [{:project/keys [target-dir class-dir scratch-dir staged-src-dir jar-file
                        elide-meta package-protocols aot-java-opts
                        allow-foreign-classes strict-foreign-classes?
                        publishable-sources strict-opacity?
                        strict-source-entries? source-namespaces
                        ship-sources? verify-load-isolated?]} project
        {:facts/keys [source-roots resource-roots namespaces preload ns-files]} facts
        protocol-namespaces (mapv #(symbol (namespace %)) package-protocols)
        compiled     (vec (remove (set source-namespaces) namespaces))
        source-files (source-namespace-files source-namespaces ns-files)]
    (into []
          (remove nil?)
          [(verify-packaged-step facts)
           {:step/kind :step/clean :step/path target-dir}
           ;; The compiler's :elide-meta reaches def metadata only, so an ns
           ;; docstring is stripped here, on a staged copy, or not at all.
           {:step/kind :step/stage-sources
            :step/src-dirs source-roots
            :step/target-dir staged-src-dir
            :step/elide-doc? (boolean (some #{:doc} elide-meta))}
           ;; Host namespaces compile FIRST in the same JVM so reify/require
           ;; against runtime-only host protocols resolves.
           {:step/kind :step/compile
            :step/src-dirs [staged-src-dir]
            :step/ns-compile (into [] (distinct)
                                   (concat protocol-namespaces preload compiled))
            :step/class-dir scratch-dir
            :step/elide-meta elide-meta
            :step/java-opts aot-java-opts}
           ;; Only this project's own namespaces are copied out of the scratch
           ;; directory. Preloaded host classes compiled alongside them must
           ;; never reach the published jar.
           {:step/kind :step/copy-classes
            :step/from scratch-dir
            :step/to class-dir
            :step/prefixes (mapv naming/ns->path compiled)
            :step/files (mapv naming/protocol->class-path package-protocols)}
           ;; :aot/source-namespaces ship as their (staged) source text.
           (when (seq source-files)
             {:step/kind :step/copy-classes
              :step/from staged-src-dir
              :step/to class-dir
              :step/prefixes []
              :step/files source-files})
           ;; What was copied is audited against what it links to: a hardcoded
           ;; foreign class the jar does not ship mounts nowhere, and says so
           ;; only when someone tries.
           {:step/kind :step/verify-classes
            :step/class-dir class-dir
            :step/prefixes (mapv naming/ns->path compiled)
            :step/allowed (or allow-foreign-classes #{})
            :step/strict? (boolean strict-foreign-classes?)}
           ;; :aot/ship-sources: the unstaged sources ride beside the classes
           ;; (cljs consumers); normalize stamps classes newer so JVM loads them.
           (when (and ship-sources? (seq source-roots))
             {:step/kind :step/copy-dir
              :step/src-dirs (vec source-roots)
              :step/target-dir class-dir})
           (when (seq resource-roots)
             {:step/kind :step/copy-dir
              :step/src-dirs (vec resource-roots)
              :step/target-dir class-dir})
           (exclude-step project class-dir)
           (when (seq resource-roots)
             {:step/kind :step/stamp-manifest
              :step/class-dir class-dir
              :step/version (released project)})
           {:step/kind :step/write-pom}
           {:step/kind :step/jar :step/class-dir class-dir :step/jar-file jar-file}
           {:step/kind :step/normalize :step/path jar-file}
           (verify-excluded-step project jar-file)
           ;; The jar is read back and compared against the sources it came
           ;; from. Elision is a step that can stop running without failing, so
           ;; whether it ran is a property of the artifact, not of the config.
           ;; A jar that ships its sources on purpose is not opaque by design.
           (when (or (not ship-sources?) strict-source-entries?)
             {:step/kind :step/verify-opacity
              :step/src-dirs source-roots
              :step/jar-file jar-file
              :step/allowed-source (into (vec publishable-sources) source-files)
              :step/strict? (boolean strict-opacity?)
              :step/strict-source? (boolean strict-source-entries?)})
           ;; Source namespaces are not required here: their optional deps are
           ;; deliberately absent from the declared classpath.
           {:step/kind :step/verify-load
            :step/jar-file jar-file
            :step/namespaces compiled
            :step/java-opts aot-java-opts
            ;; One JVM per namespace unless the project opts out: a shared
            ;; JVM lets one namespace's deps hide another's missing ones.
            :step/isolated? (not (false? verify-load-isolated?))}
           (announce (str "Built AOT " (label project) " " (released project)
                          " -> " jar-file
                          " (" (count compiled) " ns, own .class only"
                          (when (seq source-files)
                            (str "; " (count source-files) " ns as source"))
                          (when ship-sources? "; sources shipped")
                          ")"))])))

;; An uberjar is deliberately NOT a build task with a plan: `b/uber` expands
;; every dependency jar onto one class dir, and none of the release steps
;; (pom, publish, opacity audit) apply to it. It is a delivery artifact, so its
;; steps are stated here and its work is one handler call.
(defmethod steps :task/uber
  [_ project facts]
  (let [{:project/keys [target-dir class-dir uber-file uber-main uber-aot
                        uber-basis-aliases src-dirs]} project
        uber-aot (vec uber-aot)
        uber-main (some-> uber-main symbol)
        uber-basis-aliases (vec uber-basis-aliases)
        ;; b/uber's :main writes ONE manifest attribute. The class it names
        ;; exists only if compile-clj generated it, so a Main-Class naming a
        ;; namespace that was not AOT-compiled (with :gen-class in it) makes a
        ;; jar that builds, ships, and then answers ClassNotFoundException to
        ;; the first java -jar. Measured, not assumed: the first run of the
        ;; uber suite failed exactly there, so this is refused BEFORE any
        ;; effect rather than announced after one.
        main-unrunnable? (boolean (and uber-main (not (some #{uber-main} uber-aot))))]
    (if main-unrunnable?
      [{:step/kind :step/refuse
        :step/reason :uber/main-not-compiled
        :step/message
        (str "Refusing an uberjar whose Main-Class would not exist: " uber-main
             " is named as :uber/main but is not AOT-compiled, so the manifest"
             " would name a class nothing generated. Add it to :uber/aot and put"
             " (:gen-class) in its ns form, or drop :uber/main and launch with"
             " java -cp <jar> clojure.main -m.")}]
      (into []
            (remove nil?)
            [(verify-packaged-step facts)
             {:step/kind :step/clean :step/path target-dir}
             ;; Sources and resources land in class-dir first; b/uber then adds
             ;; every dependency jar's entries around them.
             {:step/kind :step/copy-dir :step/src-dirs (vec src-dirs) :step/target-dir class-dir}
             ;; AOT is OPTIONAL and default none: a record compiled against a
             ;; protocol whose library ships as source hard-references an
             ;; interface that only exists in the loader at runtime, so the
             ;; uberjar would link and then fail (axiom 20260727002436-53e3767e).
             (when (seq uber-aot)
               {:step/kind :step/compile
                :step/src-dirs (vec src-dirs)
                :step/ns-compile uber-aot
                :step/class-dir class-dir
                :step/elide-meta []
                :step/java-opts []})
             {:step/kind :step/uber
              :step/class-dir class-dir
              :step/uber-file uber-file
              :step/main uber-main
              :step/aliases uber-basis-aliases}
             (announce
              (str "Built uberjar " (label project) " " (released project)
                   " -> " uber-file
                   (when uber-main (str " (main " uber-main ")"))
                   (when (seq uber-aot)
                     (str " (" (count uber-aot) " ns AOT)"))))]))))

(defn artifact-task
  "The build task that produces `kind`."
  [kind]
  (case kind
    :artifact/aot :task/jar-aot
    :artifact/source :task/jar))

(defn compiles?
  "True when `task` on `project` will AOT-compile.

   Only a compiling task consults ./local.deps.edn, so a source-jar build never
   depends on — or fails on — a developer's local overlay file.

   An uberjar compiles only what version.edn asks for under :uber/aot, which
   defaults to nothing."
  [task project]
  (case task
    :task/jar-aot true
    :task/uber (boolean (seq (:project/uber-aot project)))
    (:task/install :task/deploy)
    (= :artifact/aot (:target/artifact-kind (publish/target (:project/target-id project))))
    false))

(defmethod steps :task/install
  [_ project facts]
  (let [target (publish/target (:project/target-id project))]
    (conj (vec (steps (artifact-task (:target/artifact-kind target)) project facts))
          {:step/kind :step/publish
           :step/target-id (:target/id target)
           :step/installer :local}
          (announce (str "Installed " (label project) " " (released project) " to ~/.m2")))))

(defmethod steps :task/deploy
  [_ project facts]
  (let [target (publish/target (:project/target-id project))
        state (:facts/registry-state facts)]
    (cond
      (not (:target/publishes? target))
      [(announce (str "Not shippable: " (label project)
                      " has :publish :none — nothing published."))]

      ;; Both registries are immutable, so a re-run is a no-op rather than an
      ;; error. The only way to release again is to bump VERSION.
      (= :complete state)
      [(announce (str "Skip: " (label project) " " (released project)
                      " already published — bump VERSION to release."))]

      (= :partial state)
      [{:step/kind :step/refuse
        :step/reason :release/half-published
        :step/message
        (str "Refusing to deploy " (label project) " " (released project)
             ": the registry holds some of this version's documents and not"
             " others. A deploy that fails between uploads leaves the"
             " coordinate resolvable and unusable, and immutability means it"
             " cannot be completed by re-running, which answers 409."
             " Delete this version from the registry, then re-run.")}]

      :else
      (conj (mapv strict-packaging
                  (steps (artifact-task (:target/artifact-kind target)) project facts))
            {:step/kind :step/publish
             :step/target-id (:target/id target)
             :step/installer :remote}
            (announce (str "Deployed " (label project) " " (released project)
                           " to " (name (:target/id target))))))))

(defn plan
  "The Plan for `task` on `project`, given `facts`."
  [task project facts]
  (vec (steps task project facts)))

(defn publishes?
  "True when running `plan` would push an artifact to a registry."
  [plan]
  (boolean (some #(and (= :step/publish (:step/kind %))
                       (= :remote (:step/installer %)))
                 plan)))

(defn builds-jar?
  "True when `plan` produces a jar."
  [plan]
  (boolean (some #(= :step/jar (:step/kind %)) plan)))

(m/=> plan [:=> [:cat s/Task s/Project s/Facts] s/Plan])
(m/=> publishes? [:=> [:cat s/Plan] :boolean])
(m/=> builds-jar? [:=> [:cat s/Plan] :boolean])
