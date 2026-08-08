(ns hive-build.promote.lint-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-schemas.test :as st]
            [hive-build.promote.lint :as lint]))

(def sep java.io.File/pathSeparator)

;; ── Synthesized from the schemas ───────────────────────────────────────────

(st/deftrifecta-from-schema lint-candidates
  hive-build.promote.lint/lint-candidates
  {:in [:vector [:enum "src" "resources" "src/synth" "dev"]]
   :out [:vector [:string {:min 1}]]
   :rel (fn [in out]
          (and (= "test" (last out)) (not (some #{"resources"} (butlast out))) (= (vec (butlast out)) (vec (remove #{"resources"} in)))))
   :mutation false
   :num-tests 200})

;; ── The argument vectors ──────────────────────────────────────────────────

(deftest lint-targets-are-sources-plus-test
  (is (= ["src" "test"] (lint/lint-candidates ["src" "resources"])))
  (is (= ["src" "src/synth" "test"] (lint/lint-candidates ["src" "resources" "src/synth"])))
  (testing "a project with no source dirs still lints its tests"
    (is (= ["test"] (lint/lint-candidates [])))))

(deftest a-classpath-drops-what-is-absent
  (is (= (str "a" sep "b") (lint/classpath ["a" "b"])))
  (is (= "a" (lint/classpath ["a" nil])))
  (is (= "a" (lint/classpath [nil "a" "" "   "])))
  (is (= "" (lint/classpath [nil nil]))))

(deftest the-sync-command-copies-dependency-configs
  (testing "macro awareness arrives with the dependency instead of being
            re-authored per repo"
    (is (= ["clj-kondo" "--lint" "cp" "--dependencies" "--parallel" "--copy-configs"]
           (lint/sync-command "cp")))))

(deftest the-lint-command-carries-the-fail-level
  (is (= ["clj-kondo" "--lint" "src" "test" "--fail-level" "error"]
         (lint/lint-command ["src" "test"] :error)))
  (is (= ["clj-kondo" "--lint" "src" "--fail-level" "warning"]
         (lint/lint-command ["src"] :warning))))

(deftest a-nil-fail-level-reports-without-failing
  (testing "the flag is absent, not empty: clj-kondo would reject --fail-level
            with no value and the lint would fail for the wrong reason"
    (let [args (lint/lint-command ["src"] nil)]
      (is (= ["clj-kondo" "--lint" "src"] args))
      (is (not-any? #{"--fail-level"} args)))))

(deftest lint-commands-never-contain-a-nil
  (testing "a nil in a command vector is a NullPointerException at process spawn"
    (doseq [args [(lint/lint-command ["src"] :error)
                  (lint/lint-command [] nil)
                  (lint/sync-command "")]]
      (is (every? string? args) (pr-str args))
      (is (not-any? str/blank? (remove #{""} args))))))
