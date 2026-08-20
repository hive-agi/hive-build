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

(defmethod steps :task/jar
  [_ project _facts]
  (let [{:project/keys [target-dir src-dirs class-dir jar-file]} project]
    [{:step/kind :step/clean :step/path target-dir}
     {:step/kind :step/write-pom}
     {:step/kind :step/copy-dir :step/src-dirs src-dirs :step/target-dir class-dir}
     {:step/kind :step/stamp-manifest :step/class-dir class-dir
      :step/version (released project)}
     {:step/kind :step/jar :step/class-dir class-dir :step/jar-file jar-file}
     {:step/kind :step/normalize :step/path jar-file}
     (announce (str "Built " (label project) " " (released project) " -> " jar-file))]))

(defmethod steps :task/jar-aot
  [_ project facts]
  (let [{:project/keys [target-dir class-dir scratch-dir staged-src-dir jar-file
                        elide-meta package-protocols aot-java-opts
                        allow-foreign-classes strict-foreign-classes?]} project
        {:facts/keys [source-roots resource-roots namespaces preload]} facts
        protocol-namespaces (mapv #(symbol (namespace %)) package-protocols)]
    (into []
          (remove nil?)
          [{:step/kind :step/clean :step/path target-dir}
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
                                   (concat protocol-namespaces preload namespaces))
            :step/class-dir scratch-dir
            :step/elide-meta elide-meta
            :step/java-opts aot-java-opts}
           ;; Only this project's own namespaces are copied out of the scratch
           ;; directory. Preloaded host classes compiled alongside them must
           ;; never reach the published jar.
           {:step/kind :step/copy-classes
            :step/from scratch-dir
            :step/to class-dir
            :step/prefixes (mapv naming/ns->path namespaces)
            :step/files (mapv naming/protocol->class-path package-protocols)}
           ;; What was copied is audited against what it links to: a hardcoded
           ;; foreign class the jar does not ship mounts nowhere, and says so
           ;; only when someone tries.
           {:step/kind :step/verify-classes
            :step/class-dir class-dir
            :step/prefixes (mapv naming/ns->path namespaces)
            :step/allowed (or allow-foreign-classes #{})
            :step/strict? (boolean strict-foreign-classes?)}
           (when (seq resource-roots)
             {:step/kind :step/copy-dir
              :step/src-dirs (vec resource-roots)
              :step/target-dir class-dir})
           (when (seq resource-roots)
             {:step/kind :step/stamp-manifest
              :step/class-dir class-dir
              :step/version (released project)})
           {:step/kind :step/write-pom}
           {:step/kind :step/jar :step/class-dir class-dir :step/jar-file jar-file}
           {:step/kind :step/normalize :step/path jar-file}
           {:step/kind :step/verify-load
            :step/jar-file jar-file
            :step/namespaces namespaces
            :step/java-opts aot-java-opts}
           (announce (str "Built AOT " (label project) " " (released project)
                          " -> " jar-file
                          " (" (count namespaces) " ns, own .class only)"))])))

(defn artifact-task
  "The build task that produces `kind`."
  [kind]
  (case kind
    :artifact/aot :task/jar-aot
    :artifact/source :task/jar))

(defn compiles?
  "True when `task` on `project` will AOT-compile.

   Only a compiling task consults ./local.deps.edn, so a source-jar build never
   depends on — or fails on — a developer's local overlay file."
  [task project]
  (case task
    :task/jar-aot true
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
  (let [target (publish/target (:project/target-id project))]
    (cond
      (not (:target/publishes? target))
      [(announce (str "Not shippable: " (label project)
                      " has :publish :none — nothing published."))]

      ;; Both registries are immutable, so a re-run is a no-op rather than an
      ;; error. The only way to release again is to bump VERSION.
      (:facts/published? facts)
      [(announce (str "Skip: " (label project) " " (released project)
                      " already published — bump VERSION to release."))]

      :else
      (conj (vec (steps (artifact-task (:target/artifact-kind target)) project facts))
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
