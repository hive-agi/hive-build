(ns hive-build.boundary.uber-test
  "The uberjar: a self-contained jar a JVM can run with nothing on its
   classpath but the jar itself.

   Asserted the only way that claim can be falsified — by running it. A jar
   that merely exists proves nothing: `b/uber` copies dependency entries
   around the class dir, and a basis that resolved fewer deps than the code
   needs still produces a file. So each test builds a FIXTURE project in a
   temp directory, then boots a fresh JVM on `-cp <jar>` and reads what it
   printed."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.tools.build.api :as b]
            [hive-build.boundary.tools :as tools]
            [hive-build.collect.proc :as proc]
            [hive-build.pipeline.plan :as plan])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util.jar JarFile)
           (java.util.zip ZipEntry ZipFile)))

(defn- temp-dir []
  (str (Files/createTempDirectory "hive-build-uber" (make-array FileAttribute 0))))

(defn- unique-ns
  "A namespace no other test run has loaded, so a stale class cannot answer."
  [prefix]
  (symbol (str prefix (System/nanoTime))))

(defn- ns-rel-path
  "The source path a namespace's file lives at."
  [ns-sym]
  (str (-> (str ns-sym) (str/replace "-" "_") (str/replace "." "/")) ".clj"))

(defn- write-source!
  "Write `text` as the source of `ns-sym` under `src-dir`."
  [src-dir ns-sym text]
  (let [f (io/file src-dir (ns-rel-path ns-sym))]
    (io/make-parents f)
    (spit f text)
    f))

(defn- run-in-jar
  "Run `form` in a fresh JVM whose whole classpath is `jar-file`.
   {:ok? :exit :out :err} as `proc/run` answers it."
  [jar-file form]
  (proc/run {} "java" "-cp" jar-file "clojure.main" "-e" form))

(defn- entry-names [jar-file]
  (with-open [zf (ZipFile. (io/file jar-file))]
    (into #{} (map #(.getName ^ZipEntry %)) (enumeration-seq (.entries zf)))))

;; ── The plan ─────────────────────────────────────────────────────────────

(def ^:private project
  {:project/coordinate {:coordinate/group-id "io.github.hive-agi"
                        :coordinate/artifact-id "hive-thing"
                        :coordinate/version "1.2.3"}
   :project/src-dirs ["src"]
   :project/target-dir "target"
   :project/class-dir "target/classes"
   :project/uber-file "target/hive-thing-1.2.3-uber.jar"})

(def ^:private facts
  {:facts/source-roots ["src"]
   :facts/resource-roots []
   :facts/namespaces ['hive-thing.core]
   :facts/preload []
   :facts/unpackaged-roots []
   :facts/registry-state :absent})

(defn- kinds [steps] (mapv :step/kind steps))

(defn- project-with
  "`project` with the uber keys the fixture omits filled in."
  [overrides]
  (merge {:project/uber-file "target/hive-thing-1.2.3-uber.jar"
          :project/uber-main nil
          :project/uber-aot []
          :project/uber-basis-aliases []}
         project overrides))

(defn- step-of [steps kind] (first (filter #(= kind (:step/kind %)) steps)))

(deftest an-uber-plan-cleans-copies-and-makes-one-jar
  (let [p (plan/plan :task/uber (project-with {}) facts)]
    (is (= [:step/clean :step/copy-dir :step/uber :step/announce] (kinds p)))
    (testing "the jar is built from the class dir, into the declared uber file"
      (is (= {:step/kind :step/uber
              :step/class-dir "target/classes"
              :step/uber-file "target/hive-thing-1.2.3-uber.jar"
              :step/main nil
              :step/aliases []}
             (step-of p :step/uber))))
    (testing "an uberjar carries no pom and publishes nowhere"
      (is (not-any? #{:step/write-pom :step/publish} (kinds p))))))

(deftest aot-is-absent-unless-the-project-asks-for-it
  (testing "the default is no AOT, because a record compiled against a
            source-only protocol lib links and then fails"
    (let [plain (project-with {})]
      (is (not-any? #{:step/compile} (kinds (plan/plan :task/uber plain facts))))
      (is (false? (plan/compiles? :task/uber plain)))))
  (testing "asking for it puts a compile step before the jar"
    (let [asking (project-with {:project/uber-aot ['hive-thing.core]})
          p (plan/plan :task/uber asking facts)]
      (is (= [:step/clean :step/copy-dir :step/compile :step/uber :step/announce] (kinds p)))
      (is (true? (plan/compiles? :task/uber asking)))
      (is (= ['hive-thing.core] (:step/ns-compile (step-of p :step/compile)))))))

(deftest a-main-that-was-never-compiled-is-refused-not-shipped
  (testing "b/uber would write a Main-Class no class answers, and the jar
            would build, publish, and fail at the first java -jar"
    (let [p (plan/plan :task/uber (project-with {:project/uber-main 'hive-thing.core}) facts)]
      (is (= :step/refuse (:step/kind (last p))))
      (is (= :uber/main-not-compiled (:step/reason (last p))))
      (is (not-any? #{:step/uber} (kinds p)) "nothing is built")))
  (testing "compiling it is the remedy"
    (let [p (plan/plan :task/uber (project-with {:project/uber-main 'hive-thing.core
                                                 :project/uber-aot ['hive-thing.core]})
                       facts)]
      (is (not-any? #{:step/refuse} (kinds p)))
      (is (= 'hive-thing.core (:step/main (step-of p :step/uber)))))))

;; ── The artifact ─────────────────────────────────────────────────────────

(deftest a-built-uberjar-runs-with-nothing-but-itself-on-the-classpath
  (let [root (temp-dir)
        src-dir (str root "/src")
        class-dir (str root "/classes")
        jar-file (str root "/thing-uber.jar")
        ns-sym (unique-ns "uber.runnable")
        marker (str "uber-ok-" (System/nanoTime))
        _ (write-source! src-dir ns-sym
                         (str "(ns " ns-sym ")\n"
                              "(defn -main [& _] (println \"" marker "\"))\n"))
        ;; The basis carries a dependency the class dir does not: data.json is
        ;; small, and a run that requires it proves dependency entries were
        ;; EXPANDED into the jar rather than resolved from disk.
        basis (tools/uber-basis {:paths [src-dir]
                                 :deps {'org.clojure/data.json {:mvn/version "2.5.2"}}}
                                [])
        step {:step/kind :step/uber :step/class-dir class-dir
              :step/uber-file jar-file :step/main nil :step/aliases []}]
    (b/copy-dir {:src-dirs [src-dir] :target-dir class-dir})
    ((get tools/handlers :step/uber) {:ctx/uber-basis-fn (fn [_aliases] basis)} step)
    (testing "the jar exists and carries the project's own source"
      (is (.isFile (io/file jar-file)))
      (is (contains? (entry-names jar-file) (ns-rel-path ns-sym))))
    (testing "a JVM runs it with only the jar on its classpath"
      (let [r (run-in-jar jar-file (str "(require '" ns-sym ") (" ns-sym "/-main)"))]
        (is (zero? (:exit r)) (str "stderr: " (:err r)))
        (is (str/includes? (:out r) marker))))
    (testing "a dependency's entries were expanded into the jar"
      (is (contains? (entry-names jar-file) "clojure/data/json.clj"))
      (let [r (run-in-jar jar-file "(require 'clojure.data.json) (println :json-loaded)")]
        (is (zero? (:exit r)) (str "stderr: " (:err r)))
        (is (str/includes? (:out r) ":json-loaded"))))))

(deftest a-named-main-namespaces-the-manifest
  (let [root (temp-dir)
        src-dir (str root "/src")
        class-dir (str root "/classes")
        jar-file (str root "/main-uber.jar")
        ns-sym (unique-ns "uber.main")
        _ (write-source! src-dir ns-sym
                         (str "(ns " ns-sym " (:gen-class))\n"
                              "(defn -main [& _] (println \"main-ran\"))\n"))
        basis (tools/uber-basis {:paths [src-dir]} [])
        step {:step/kind :step/uber :step/class-dir class-dir
              :step/uber-file jar-file :step/main ns-sym :step/aliases []}]
    (b/copy-dir {:src-dirs [src-dir] :target-dir class-dir})
    ;; A manifest Main-Class names a CLASS, so the namespace has to be
    ;; compiled. Without this, b/uber still writes the attribute and the jar
    ;; still builds \u2014 it just cannot be run, which is the plan refusal above.
    (b/compile-clj {:basis basis :src-dirs [src-dir]
                    :ns-compile [ns-sym] :class-dir class-dir})
    ((get tools/handlers :step/uber) {:ctx/uber-basis-fn (fn [_] basis)} step)
    (testing "Main-Class is written, with the dash-to-underscore gen-class name"
      (with-open [jf (JarFile. (io/file jar-file))]
        (is (= (str/replace (str ns-sym) "-" "_")
               (.getValue (.getMainAttributes (.getManifest jf)) "Main-Class")))))
    (testing "the jar runs with no classpath at all"
      (let [r (proc/run {} "java" "-jar" jar-file)]
        (is (zero? (:exit r)) (str "stderr: " (:err r)))
        (is (str/includes? (:out r) "main-ran"))))))

;; ── The handler hands tools.build the keys it reads ──────────────────────

(def uber-options
  "Keys clojure.tools.build.api/uber documents. Anything else is discarded
   without a word."
  #{:uber-file :class-dir :basis :main :manifest :exclude :conflict-handlers})

(defn- captured-uber [step]
  (let [captured (atom nil)]
    (with-redefs [b/uber (fn [opts] (reset! captured opts) nil)]
      ((get tools/handlers :step/uber)
       {:ctx/uber-basis-fn (fn [_] ::basis)} step))
    @captured))

(deftest no-option-is-passed-that-tools-build-would-discard
  (let [opts (captured-uber {:step/kind :step/uber :step/class-dir "target/classes"
                             :step/uber-file "target/x-uber.jar"
                             :step/main 'x.core :step/aliases [:dev]})
        unknown (set/difference (set (keys opts)) uber-options)]
    (is (empty? unknown) (str "tools.build will ignore: " unknown))
    (is (= {:class-dir "target/classes" :uber-file "target/x-uber.jar"
            :basis ::basis :main 'x.core}
           opts))))

(deftest main-is-omitted-rather-than-nil
  (testing "a nil :main would be written as a Main-Class named nil"
    (is (not (contains? (captured-uber {:step/kind :step/uber :step/class-dir "c"
                                        :step/uber-file "j" :step/main nil
                                        :step/aliases []})
                        :main)))))

(deftest the-basis-is-resolved-on-the-declared-aliases
  (let [seen (atom nil)]
    ((get tools/handlers :step/uber)
     {:ctx/uber-basis-fn (fn [aliases] (reset! seen aliases) ::basis)}
     {:step/kind :step/uber :step/class-dir "c" :step/uber-file "j"
      :step/main nil :step/aliases [:dev :test]})
    (is (= [:dev :test] @seen))))
