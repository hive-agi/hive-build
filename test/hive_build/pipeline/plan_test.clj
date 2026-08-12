(ns hive-build.pipeline.plan-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [hive-build.pipeline.plan :as plan]
            [hive-build.promote.publish :as publish]
            [hive-build.schema :as s]))

;; ── Fixtures ──────────────────────────────────────────────────────────────

(def coordinate
  {:coordinate/group-id "io.github.hive-agi"
   :coordinate/artifact-id "hive-thing"
   :coordinate/version "1.2.3"})

(defn project
  [target-id & {:as overrides}]
  (merge {:project/coordinate coordinate
          :project/src-dirs ["src" "resources"]
          :project/target-dir "target"
          :project/class-dir "target/classes"
          :project/scratch-dir "target/aot-classes"
          :project/jar-file "target/hive-thing-1.2.3.jar"
          :project/target-id target-id
          :project/license {:license/name "MIT" :license/url ""}
          :project/scm-url "https://github.com/hive-agi/hive-thing"
          :project/elide-meta [:doc :file :line]
          :project/pom-exclude-deps #{}
          :project/package-protocols []
          :project/aot-java-opts []}
         overrides))

(def facts
  {:facts/source-roots ["src"]
   :facts/resource-roots ["resources"]
   :facts/namespaces ['hive-thing.core 'hive-thing.impl]
   :facts/preload ['host.protocol]
   :facts/published? false})

(def s3
  {:target/id :s3
   :target/artifact-kind :artifact/aot
   :target/publishes? true
   :target/repo-url nil
   :target/repo-url-env "S3_MAVEN_URL"
   :target/repository-name "s3"
   :target/username-env "S3_KEY_ID"
   :target/password-env "S3_SECRET"})

(use-fixtures :each (fn [t] (try (t) (finally (publish/deregister! :s3)))))

(defn kinds [steps] (mapv :step/kind steps))

(defn step-of [steps kind] (first (filter #(= kind (:step/kind %)) steps)))

(defn index-of [steps kind]
  (first (keep-indexed (fn [i st] (when (= kind (:step/kind st)) i)) steps)))

;; ── Every plan is a well-formed value ─────────────────────────────────────

(deftest every-plan-conforms-to-the-plan-schema
  (doseq [target-id (publish/target-ids)
          task [:task/clean :task/jar :task/jar-aot :task/install :task/deploy]
          published? [true false]]
    (let [p (plan/plan task (project target-id) (assoc facts :facts/published? published?))]
      (is (nil? (m/explain s/Plan p))
          (str task " / " target-id " / published? " published?)))))

(deftest an-unknown-task-has-no-plan
  (is (thrown? clojure.lang.ExceptionInfo
               (plan/plan :task/publish-to-npm (project :none) facts))))

(deftest only-a-compiling-task-needs-the-local-overlay
  (testing "a source-jar build must not depend on — or fail on — a developer's
            gitignored local.deps.edn"
    (is (not (plan/compiles? :task/jar (project :gitea))))
    (is (not (plan/compiles? :task/clean (project :gitea))))
    (is (plan/compiles? :task/jar-aot (project :clojars)))
    (testing "install and deploy follow the target's artifact kind"
      (is (plan/compiles? :task/deploy (project :gitea)))
      (is (plan/compiles? :task/install (project :gitea)))
      (is (not (plan/compiles? :task/deploy (project :clojars))))
      (is (not (plan/compiles? :task/deploy (project :gitea-source))))
      (is (not (plan/compiles? :task/deploy (project :none)))))))

(deftest compiles-agrees-with-the-plan-it-describes
  (testing "the overlay is read exactly when a plan contains a compile step"
    (doseq [target-id (publish/target-ids)
            task [:task/clean :task/jar :task/jar-aot :task/install :task/deploy]]
      (let [p (plan/plan task (project target-id) facts)
            has-compile? (some #(= :step/compile (:step/kind %)) p)]
        (is (= (boolean has-compile?) (plan/compiles? task (project target-id)))
            (str task " / " target-id))))))

;; ── The build plans ───────────────────────────────────────────────────────

(deftest the-source-jar-plan-is-exactly-this
  (is (= [:step/clean :step/write-pom :step/copy-dir :step/stamp-manifest :step/jar
          :step/normalize :step/announce]
         (kinds (plan/plan :task/jar (project :clojars) facts)))))

(deftest the-source-jar-copies-every-src-dir-including-resources
  (is (= {:step/kind :step/copy-dir
          :step/src-dirs ["src" "resources"]
          :step/target-dir "target/classes"}
         (step-of (plan/plan :task/jar (project :clojars) facts) :step/copy-dir))))

(deftest the-aot-jar-plan-is-exactly-this
  (is (= [:step/clean :step/compile :step/copy-classes :step/verify-classes
          :step/copy-dir :step/stamp-manifest :step/write-pom :step/jar
          :step/normalize :step/verify-load :step/announce]
         (kinds (plan/plan :task/jar-aot (project :gitea) facts)))))

(deftest the-aot-jar-copies-only-resource-roots
  (testing "sources are compiled, never packaged"
    (is (= {:step/kind :step/copy-dir
            :step/src-dirs ["resources"]
            :step/target-dir "target/classes"}
           (step-of (plan/plan :task/jar-aot (project :gitea) facts) :step/copy-dir)))))

(deftest a-project-with-no-resources-has-no-copy-step
  (let [p (plan/plan :task/jar-aot (project :gitea) (assoc facts :facts/resource-roots []))]
    (is (= [:step/clean :step/compile :step/copy-classes :step/verify-classes
            :step/write-pom :step/jar :step/normalize :step/verify-load :step/announce]
           (kinds p)))))

(deftest the-aot-audit-defaults-to-reporting-not-failing
  (testing "a project that declares nothing still plans a total step"
    (let [step (step-of (plan/plan :task/jar-aot (project :gitea) facts) :step/verify-classes)]
      (is (= #{} (:step/allowed step)))
      (is (false? (:step/strict? step)))
      (is (= ["hive_thing/core" "hive_thing/impl"] (:step/prefixes step)))))
  (testing "declaring strictness turns the report into a failure"
    (let [strict (assoc (project :gitea)
                        :project/strict-foreign-classes? true
                        :project/allow-foreign-classes #{"hive_spi/memory/ports/IMemoryStore"})
          step   (step-of (plan/plan :task/jar-aot strict facts) :step/verify-classes)]
      (is (true? (:step/strict? step)))
      (is (= #{"hive_spi/memory/ports/IMemoryStore"} (:step/allowed step))))))

;; ── The leak guard ────────────────────────────────────────────────────────

(deftest preloaded-host-namespaces-compile-but-are-never-packaged
  (let [p (plan/plan :task/jar-aot (project :gitea) facts)
        compile-step (step-of p :step/compile)
        copy-step (step-of p :step/copy-classes)]
    (testing "the host namespace is compiled, and compiled first"
      (is (= ['host.protocol 'hive-thing.core 'hive-thing.impl]
             (:step/ns-compile compile-step))))
    (testing "and its classes are not admitted to the jar"
      (is (= ["hive_thing/core" "hive_thing/impl"] (:step/prefixes copy-step)))
      (is (= [] (:step/files copy-step)))
      (is (not-any? #(str/includes? % "host") (:step/prefixes copy-step))))))

(deftest explicitly-packaged-protocol-interfaces-enter-the-jar
  (let [p (plan/plan :task/jar-aot
                     (project :gitea
                              :project/package-protocols
                              ['hive-addon.protocol/IAddon])
                     facts)
        compile-step (step-of p :step/compile)
        copy-step (step-of p :step/copy-classes)]
    (is (= 'hive-addon.protocol (first (:step/ns-compile compile-step))))
    (is (= ["hive_addon/protocol/IAddon.class"] (:step/files copy-step)))
    (is (not-any? #(str/includes? % "hive_addon") (:step/prefixes copy-step)))))

(tc/defspec no-preloaded-namespace-ever-reaches-the-jar 200
  (prop/for-all [own (gen/vector-distinct (gen/elements '[a.one a.two b.three]) {:max-elements 3})
                 preload (gen/vector-distinct (gen/elements '[host.x host.y vendor.z])
                                              {:max-elements 3})]
    (let [p (plan/plan :task/jar-aot (project :gitea)
                       (assoc facts :facts/namespaces (vec own) :facts/preload (vec preload)))
          copy-step (step-of p :step/copy-classes)
          prefixes (:step/prefixes copy-step)
          files (:step/files copy-step)
          compiled (:step/ns-compile (step-of p :step/compile))]
      (and (= (count prefixes) (count own))
           (empty? files)
           ;; every preloaded ns is compiled
           (every? (set compiled) preload)
           ;; and none of them is a packaging prefix
           (not-any? (fn [ns-sym]
                       (let [path (str/replace (str ns-sym) "." "/")]
                         (some #(= path %) prefixes)))
                     preload)))))

(deftest elision-and-java-opts-reach-the-compile-step
  (let [p (plan/plan :task/jar-aot
                     (project :gitea
                              :project/elide-meta [:doc]
                              :project/aot-java-opts ["-Xmx2g"])
                     facts)
        step (step-of p :step/compile)]
    (is (= [:doc] (:step/elide-meta step)))
    (is (= ["-Xmx2g"] (:step/java-opts step)))
    (is (= "target/aot-classes" (:step/class-dir step)))))

(deftest the-built-aot-jar-is-verified-on-its-declared-classpath
  (let [p (plan/plan :task/jar-aot
                     (project :gitea :project/aot-java-opts ["--add-modules=x"])
                     facts)
        step (step-of p :step/verify-load)]
    (is (= "target/hive-thing-1.2.3.jar" (:step/jar-file step)))
    (is (= ['hive-thing.core 'hive-thing.impl] (:step/namespaces step)))
    (is (= ["--add-modules=x"] (:step/java-opts step)))
    (is (< (index-of p :step/normalize) (index-of p :step/verify-load)))
    (is (< (index-of p :step/verify-load) (index-of p :step/announce)))))

;; ── The publish decision ──────────────────────────────────────────────────

(deftest a-package-that-does-not-ship-builds-nothing
  (let [p (plan/plan :task/deploy (project :none) facts)]
    (is (= [:step/announce] (kinds p)))
    (is (not (plan/builds-jar? p)))
    (is (not (plan/publishes? p)))
    (is (re-find #"Not shippable" (:step/message (first p))))))

(deftest an-already-published-coordinate-is-a-no-op-not-an-error
  (testing "both registries are immutable, so releasing again means bumping VERSION"
    (doseq [target-id [:clojars :gitea :gitea-source]]
      (let [p (plan/plan :task/deploy (project target-id) (assoc facts :facts/published? true))]
        (is (= [:step/announce] (kinds p)) (str target-id))
        (is (not (plan/publishes? p)))
        (is (re-find #"already published" (:step/message (first p))))
        (is (str/includes? (:step/message (first p)) "1.2.3"))))))

(deftest clojars-receives-the-source-jar
  (let [p (plan/plan :task/deploy (project :clojars) facts)]
    (is (= [:step/clean :step/write-pom :step/copy-dir :step/stamp-manifest :step/jar
            :step/normalize :step/announce :step/publish :step/announce]
           (kinds p)))
    (is (plan/publishes? p))))

(deftest the-private-registry-receives-the-aot-jar
  (let [p (plan/plan :task/deploy (project :gitea) facts)]
    (is (= :step/compile (second (kinds p))))
    (is (= :step/copy-classes (nth (kinds p) 2)))
    (is (plan/publishes? p))
    (is (= {:step/kind :step/publish :step/target-id :gitea :step/installer :remote}
           (step-of p :step/publish)))))

(deftest gitea-source-is-the-deliberate-exception
  (let [p (plan/plan :task/deploy (project :gitea-source) facts)]
    (testing "same destination, source jar"
      (is (not-any? #{:step/compile :step/copy-classes} (kinds p)))
      (is (= :gitea-source (:step/target-id (step-of p :step/publish)))))))

(deftest an-install-is-local-and-never-reaches-a-registry
  (doseq [target-id (publish/target-ids)]
    (let [p (plan/plan :task/install (project target-id) facts)]
      (is (= :local (:step/installer (step-of p :step/publish))) (str target-id))
      (is (not (plan/publishes? p)) (str target-id))
      (is (plan/builds-jar? p) (str target-id)))))

(deftest install-follows-the-targets-artifact-kind
  (testing "what you install locally is what the registry would receive"
    (is (some #{:step/compile} (kinds (plan/plan :task/install (project :gitea) facts))))
    (is (not-any? #{:step/compile} (kinds (plan/plan :task/install (project :clojars) facts))))))

;; ── Structural invariants that hold for every plan ────────────────────────

(deftest a-jar-is-always-normalized-immediately-after-it-is-built
  (testing "a jar that escaped normalization is nondeterministic, and any two
            copies of it differ for reasons that are not content"
    (doseq [target-id (publish/target-ids)
            task [:task/jar :task/jar-aot :task/install :task/deploy]]
      (let [p (plan/plan task (project target-id) facts)]
        (doseq [[i step] (map-indexed vector p)
                :when (= :step/jar (:step/kind step))]
          (let [next-step (get p (inc i))]
            (is (= :step/normalize (:step/kind next-step)) (str task " / " target-id))
            (is (= (:step/jar-file step) (:step/path next-step)))))))))

(deftest nothing-is-published-that-was-not-just-built
  (doseq [target-id (publish/target-ids)
          task [:task/install :task/deploy]]
    (let [p (plan/plan task (project target-id) facts)]
      (when-let [publish-at (index-of p :step/publish)]
        (is (some? (index-of p :step/jar)) (str task " / " target-id))
        (is (< (index-of p :step/jar) publish-at))
        (is (< (index-of p :step/normalize) publish-at))))))

(deftest any-plan-that-writes-to-target-cleans-it-first
  (doseq [target-id (publish/target-ids)
          task [:task/clean :task/jar :task/jar-aot :task/install :task/deploy]]
    (let [p (plan/plan task (project target-id) facts)]
      (when (plan/builds-jar? p)
        (is (= :step/clean (:step/kind (first p))) (str task " / " target-id))
        (is (= "target" (:step/path (first p))))))))

(deftest a-pom-is-written-before-every-jar
  (doseq [target-id (publish/target-ids)
          task [:task/jar :task/jar-aot :task/install :task/deploy]]
    (let [p (plan/plan task (project target-id) facts)]
      (when (plan/builds-jar? p)
        (is (< (index-of p :step/write-pom) (index-of p :step/jar))
            (str task " / " target-id))))))

(deftest every-announcement-names-the-coordinate
  (testing "build output is the only record of what a CI run actually shipped"
    (doseq [target-id (publish/target-ids)
            task [:task/jar :task/jar-aot :task/install :task/deploy]]
      (doseq [step (filter #(= :step/announce (:step/kind %))
                           (plan/plan task (project target-id) facts))]
        (is (str/includes? (:step/message step) "hive-thing")
            (str task " / " target-id " / " (:step/message step)))))))

;; ── Open for extension ────────────────────────────────────────────────────

(deftest a-newly-registered-destination-plans-without-editing-the-planner
  (publish/register! s3)
  (let [p (plan/plan :task/deploy (project :s3) facts)]
    (is (nil? (m/explain s/Plan p)))
    (is (plan/publishes? p))
    (testing "its artifact kind decides which jar, with no branch naming :s3"
      (is (some #{:step/compile} (kinds p))))
    (is (= :s3 (:step/target-id (step-of p :step/publish))))))

(deftest a-registered-destination-that-does-not-ship-never-builds
  (publish/register! (assoc s3 :target/publishes? false))
  (is (= [:step/announce] (kinds (plan/plan :task/deploy (project :s3) facts)))))

;; ── The deploy decision, generatively ─────────────────────────────────────

(tc/defspec deploy-publishes-exactly-when-the-target-ships-a-fresh-coordinate 200
  (prop/for-all [target-id (gen/elements (vec (publish/target-ids)))
                 published? gen/boolean]
    (let [target (publish/target target-id)
          p (plan/plan :task/deploy (project target-id)
                       (assoc facts :facts/published? published?))
          should-publish? (and (:target/publishes? target) (not published?))]
      (and (= should-publish? (plan/publishes? p))
           (= should-publish? (plan/builds-jar? p))
           (nil? (m/explain s/Plan p))))))
