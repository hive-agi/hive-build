(ns hive-build.pipeline.plan-exclude-test
  "version.edn :jar-excludes reaches the plan as a prune before the jar is
   written and a read-back after it, in both jar shapes; a project that
   excludes nothing plans neither."
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [hive-build.pipeline.plan :as plan]
            [hive-build.pipeline.plan-test :as fixture :refer [facts kinds step-of]]
            [hive-build.schema :as s]))

(def excludes ["hive_thing/addon.clj" "META-INF/hive-addons"])

(defn- project [target-id]
  (fixture/project target-id :project/jar-excludes excludes))

(defn- index-of [steps kind]
  (first (keep-indexed (fn [i step] (when (= kind (:step/kind step)) i)) steps)))

(deftest the-source-jar-prunes-after-the-copy-and-verifies-after-the-jar
  (let [p (plan/plan :task/jar (project :clojars) facts)]
    (is (= [:step/clean :step/write-pom :step/copy-dir :step/exclude
            :step/stamp-manifest :step/jar :step/normalize :step/verify-excluded
            :step/announce]
           (kinds p)))
    (is (= {:step/kind :step/exclude
            :step/class-dir "target/classes"
            :step/paths excludes}
           (step-of p :step/exclude)))
    (is (= {:step/kind :step/verify-excluded
            :step/jar-file "target/hive-thing-1.2.3.jar"
            :step/paths excludes}
           (step-of p :step/verify-excluded)))
    (is (nil? (m/explain s/Plan p)))))

(deftest the-aot-jar-prunes-what-was-copied-and-verifies-the-artifact
  (let [p (plan/plan :task/jar-aot (project :gitea) facts)]
    (testing "the prune follows every copy into class-dir and precedes the jar"
      (is (< (index-of p :step/copy-classes) (index-of p :step/exclude)))
      (is (< (index-of p :step/copy-dir) (index-of p :step/exclude)))
      (is (< (index-of p :step/exclude) (index-of p :step/jar))))
    (testing "the read-back follows the jar and its normalization"
      (is (< (index-of p :step/normalize) (index-of p :step/verify-excluded))))
    (is (nil? (m/explain s/Plan p)))))

(deftest a-project-that-excludes-nothing-plans-neither-step
  (doseq [[task target] [[:task/jar :clojars] [:task/jar-aot :gitea]]]
    (let [p (plan/plan task (fixture/project target :project/jar-excludes []) facts)]
      (is (nil? (step-of p :step/exclude)) (str task))
      (is (nil? (step-of p :step/verify-excluded)) (str task)))))

(deftest deploy-and-install-inherit-the-exclusion
  (doseq [task [:task/install :task/deploy]]
    (let [p (plan/plan task (project :clojars) facts)]
      (is (some? (step-of p :step/exclude)) (str task))
      (is (some? (step-of p :step/verify-excluded)) (str task)))))
