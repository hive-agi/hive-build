(ns hive-build.promote.project-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [hive-schemas.test :as st]
            [hive-build.promote.project :as project]
            [hive-build.schema :as s]
            [clojure.set :as set]))

(def minimal-cfg
  {:lib 'io.github.hive-agi/hive-thing})

(def full-cfg
  {:lib 'io.github.hive-agi/hive-thing
   :license {:name "MIT" :url "https://opensource.org/licenses/MIT"}
   :scm-url "https://github.com/hive-agi/hive-thing"
   :src-dirs ["src" "resources"]
   :publish :gitea
   :aot/java-opts ["-Xmx2g"]
   :aot/elide-meta [:doc]
   :pom-exclude-deps ['some/host-lib]})

;; ── Synthesized from the schemas ───────────────────────────────────────────

(st/deftrifecta-from-schema source-file?
  hive-build.promote.project/source-file?
  {:in [:enum "src/a.clj" "src/a.cljc" "src/a.cljs" "src/a.edn" "README.md"
        "resources/clj-kondo.exports/x/y/config.clj" "src/a.class"]
   :out :boolean
   :rel (fn [in out] (= out (contains? #{"src/a.clj" "src/a.cljc" "src/a.cljs"} in)))
   :mutation false
   :num-tests 100})

;; ── Defaults ──────────────────────────────────────────────────────────────

(deftest a-minimal-config-yields-a-complete-project
  (let [p (project/project minimal-cfg "1.2.3")]
    (is (nil? (m/explain s/Project p)))
    (testing "every default is applied exactly once, here"
      (is (= ["src"] (:project/src-dirs p)))
      (is (= "target" (:project/target-dir p)))
      (is (= "target/classes" (:project/class-dir p)))
      (is (= "target/aot-classes" (:project/scratch-dir p)))
      (is (= "target/hive-thing-1.2.3.jar" (:project/jar-file p)))
      (is (= project/default-elide-meta (:project/elide-meta p)))
      (is (= #{} (:project/pom-exclude-deps p)))
      (is (= [] (:project/aot-java-opts p))))
    (testing "a package says nothing about publishing until it opts in"
      (is (= :none (:project/target-id p))))
    (testing "an undeclared licence stays nil rather than becoming a guess"
      (is (nil? (:project/license p))))))

(deftest a-full-config-is-carried-through
  (let [p (project/project full-cfg "0.9.1")]
    (is (nil? (m/explain s/Project p)))
    (is (= {:coordinate/group-id "io.github.hive-agi"
            :coordinate/artifact-id "hive-thing"
            :coordinate/version "0.9.1"}
           (:project/coordinate p)))
    (is (= ["src" "resources"] (:project/src-dirs p)))
    (is (= :gitea (:project/target-id p)))
    (is (= {:license/name "MIT" :license/url "https://opensource.org/licenses/MIT"}
           (:project/license p)))
    (is (= [:doc] (:project/elide-meta p)))
    (is (= #{'some/host-lib} (:project/pom-exclude-deps p)))
    (is (= ["-Xmx2g"] (:project/aot-java-opts p)))))

(deftest elide-meta-can-be-disabled-but-not-by-accident
  (testing "an explicit empty vector disables elision; an absent key does not"
    (is (= [] (:project/elide-meta (project/project (assoc minimal-cfg :aot/elide-meta [])
                                                    "1.0.0"))))
    (is (seq (:project/elide-meta (project/project minimal-cfg "1.0.0"))))))

(deftest a-nameless-licence-is-no-licence
  (testing "a :license map without a usable :name must not reach the pom"
    (doseq [bad [{} {:name ""} {:name "   "} {:url "https://x"} {:name nil}]]
      (is (nil? (:project/license (project/project (assoc minimal-cfg :license bad)
                                                   "1.0.0")))))))

(deftest a-missing-lib-stops-the-build
  (testing "a build that guesses its own coordinate is worse than one that stops"
    (doseq [bad [nil 'unqualified "io.github/x" :io.github/x 42]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (project/project (assoc minimal-cfg :lib bad) "1.0.0"))
          (str "accepted :lib " (pr-str bad))))))

;; ── Root classification ───────────────────────────────────────────────────

(deftest roots-are-classified-by-what-they-contain
  (let [dirs ["src" "resources" "empty"]
        files {"src" ["src/a/b.clj" "src/a/c.cljc"]
               "resources" ["resources/logo.png" "resources/config.edn"]
               "empty" []}]
    (is (= {:facts/source-roots ["src"]
            :facts/resource-roots ["resources" "empty"]}
           (project/classify-roots dirs files)))))

(deftest a-kondo-export-root-is-not-a-source-root
  (testing "exported hooks are shipped, never compiled"
    (is (= {:facts/source-roots []
            :facts/resource-roots ["resources"]}
           (project/classify-roots
            ["resources"]
            {"resources" ["resources/clj-kondo.exports/g/a/hooks/x.clj"]})))))

(tc/defspec classification-partitions-the-source-dirs 200
  (prop/for-all [dirs (gen/vector-distinct (gen/elements ["src" "src/synth" "resources" "dev"])
                                           {:min-elements 0 :max-elements 4})
                 seeds (gen/vector (gen/elements [[] ["x.clj"] ["x.png"] ["x.cljc" "y.md"]])
                                   0 4)]
    (let [files (zipmap dirs (concat seeds (repeat [])))
          {:facts/keys [source-roots resource-roots]} (project/classify-roots (vec dirs) files)]
      (and (= (set dirs) (into (set source-roots) resource-roots))
           (empty? (set/intersection (set source-roots) (set resource-roots)))
           (= (count dirs) (+ (count source-roots) (count resource-roots)))))))
