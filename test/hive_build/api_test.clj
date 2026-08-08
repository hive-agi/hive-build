(ns hive-build.api-test
  "The task entry points, with their effects substituted."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-build.api :as api]
            [hive-build.boundary.tools :as tools]
            [hive-build.collect.io :as io']
            [clojure.string :as str]))

;; ── bump ──────────────────────────────────────────────────────────────────

(deftest bump-rewrites-the-version-file
  (let [written (atom nil)]
    (with-redefs [io'/read-text (fn [path] (when (= "VERSION" path) "0.3.10\n"))
                  io'/write-text! (fn [path text] (reset! written [path text]) path)]
      (is (= "0.3.11" (api/bump {})))
      (is (= ["VERSION" "0.3.11\n"] @written))
      (testing "the file keeps its trailing newline"
        (is (str/ends-with? (second @written) "\n"))))))

(deftest bump-honours-the-level
  (with-redefs [io'/read-text (constantly "1.4.9\n")
                io'/write-text! (fn [_ _] nil)]
    (is (= "1.4.10" (api/bump {:level :patch})))
    (is (= "1.5.0" (api/bump {:level :minor})))
    (is (= "2.0.0" (api/bump {:level :major})))))

(deftest bump-defaults-to-patch
  (with-redefs [io'/read-text (constantly "1.4.9\n")
                io'/write-text! (fn [_ _] nil)]
    (is (= (api/bump {:level :patch}) (api/bump {})))))

(deftest bump-refuses-without-a-version-file
  (with-redefs [io'/read-text (constantly nil)]
    (is (thrown? clojure.lang.ExceptionInfo (api/bump {}))))
  (with-redefs [io'/read-text (constantly "   \n")]
    (is (thrown? clojure.lang.ExceptionInfo (api/bump {})))))

(deftest bump-never-writes-a-version-it-could-not-compute
  (let [written (atom nil)]
    (with-redefs [io'/read-text (constantly "not-a-version\n")
                  io'/write-text! (fn [p t] (reset! written [p t]) p)]
      (is (thrown? clojure.lang.ExceptionInfo (api/bump {})))
      (is (nil? @written) "VERSION must be left as it was"))))

;; ── kondo ─────────────────────────────────────────────────────────────────

(defn- capture-kondo
  "Run the kondo task, recording which process runner each command went
   through."
  [{:keys [exit opts]}]
  (let [calls (atom [])]
    (with-redefs [tools/kondo-available? (constantly true)
                  tools/deps-classpath (constantly "cp")
                  tools/bb-classpath (constantly nil)
                  tools/probe-process (fn [args] (swap! calls conj [:probe args]) {:exit 0})
                  tools/run-process (fn [args] (swap! calls conj [:run args]) {:exit exit})]
      (let [result (try (api/kondo (or opts {})) (catch clojure.lang.ExceptionInfo e e))]
        {:calls @calls :result result}))))

(deftest lint-findings-reach-the-console
  (testing "a linter whose output was captured and discarded is a linter that
            always passes"
    (let [{:keys [calls]} (capture-kondo {:exit 0})]
      (is (= 2 (count calls)))
      (is (every? #(= :run (first %)) calls)
          "both clj-kondo invocations must stream their output"))))

(deftest lint-syncs-dependency-configs-before-linting
  (let [{:keys [calls]} (capture-kondo {:exit 0})
        [sync-args lint-args] (map second calls)]
    (is (some #{"--copy-configs"} sync-args))
    (is (some #{"--dependencies"} sync-args))
    (is (not-any? #{"--copy-configs"} lint-args))
    (is (some #{"--fail-level" } lint-args))))

(deftest a-findings-exit-fails-the-build-when-a-fail-level-is-set
  (let [{:keys [result]} (capture-kondo {:exit 3})]
    (is (instance? clojure.lang.ExceptionInfo result))
    (is (= {:fail-level :error :exit 3} (ex-data result)))))

(deftest a-nil-fail-level-reports-without-failing
  (let [{:keys [result calls]} (capture-kondo {:exit 3 :opts {:fail-level nil}})]
    (is (= {:exit 3} result))
    (is (not-any? #{"--fail-level"} (second (second calls))))))

(deftest a-clean-lint-passes
  (is (= {:exit 0} (:result (capture-kondo {:exit 0})))))

(deftest lint-is-skipped-when-clj-kondo-is-absent
  (testing "a missing linter is a skip, not a failed release"
    (let [ran (atom false)]
      (with-redefs [tools/kondo-available? (constantly false)
                    tools/run-process (fn [_] (reset! ran true) {:exit 0})]
        (is (nil? (api/kondo {})))
        (is (not @ran))))))

(deftest explicit-paths-override-the-defaults
  (let [{:keys [calls]} (capture-kondo {:exit 0 :opts {:paths ["src/hive_build"]}})
        lint-args (second (second calls))]
    (is (some #{"src/hive_build"} lint-args))
    (is (not-any? #{"test"} lint-args))))
