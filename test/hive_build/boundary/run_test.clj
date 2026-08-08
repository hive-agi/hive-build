(ns hive-build.boundary.run-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-build.boundary.run :as run]))

(defn recorder
  "Handlers for `kinds` that record every step into `log` instead of running
   it."
  [log kinds]
  (into {} (map (fn [k] [k (fn [ctx step] (swap! log conj [k step (:ctx/tag ctx)]) k)])) kinds))

(def sample-plan
  [{:step/kind :step/clean :step/path "target"}
   {:step/kind :step/jar :step/class-dir "target/classes" :step/jar-file "target/x.jar"}
   {:step/kind :step/normalize :step/path "target/x.jar"}
   {:step/kind :step/announce :step/message "done"}])

(def all-kinds (into #{} (map :step/kind) sample-plan))

(deftest coverage-is-checked-before-anything-happens
  (testing "a release that ran half its steps and reported success is the
            failure this design exists to prevent"
    (let [log (atom [])
          handlers (recorder log (disj all-kinds :step/normalize))]
      (is (thrown? clojure.lang.ExceptionInfo (run/run! handlers {} sample-plan)))
      (is (empty? @log) "the clean and jar steps must not have run"))))

(deftest the-missing-kinds-are-named
  (let [handlers (recorder (atom []) #{:step/clean})]
    (is (= [:step/jar :step/normalize :step/announce]
           (run/missing-handlers handlers sample-plan)))
    (is (not (run/runnable? handlers sample-plan)))))

(deftest a-missing-kind-is-reported-once-however-often-it-appears
  (let [plan (into sample-plan sample-plan)]
    (is (= [:step/jar :step/normalize :step/announce]
           (run/missing-handlers (recorder (atom []) #{:step/clean}) plan)))))

(deftest full-coverage-is-runnable
  (is (run/runnable? (recorder (atom []) all-kinds) sample-plan))
  (is (= [] (run/missing-handlers (recorder (atom []) all-kinds) sample-plan))))

(deftest an-empty-plan-is-runnable-by-any-handler-set
  (is (run/runnable? {} []))
  (is (= [] (run/run! {} {} []))))

(deftest steps-execute-in-plan-order
  (let [log (atom [])
        results (run/run! (recorder log all-kinds) {:ctx/tag :ctx} sample-plan)]
    (is (= [:step/clean :step/jar :step/normalize :step/announce] (mapv first @log)))
    (is (= sample-plan (mapv second @log)))
    (is (= [:step/clean :step/jar :step/normalize :step/announce] results))))

(deftest the-context-reaches-every-handler
  (let [log (atom [])]
    (run/run! (recorder log all-kinds) {:ctx/tag :injected} sample-plan)
    (is (every? #(= :injected (nth % 2)) @log))))

(deftest a-throwing-handler-stops-the-run
  (let [log (atom [])
        handlers (assoc (recorder log all-kinds)
                        :step/jar (fn [_ _] (throw (ex-info "jar failed" {}))))]
    (is (thrown? clojure.lang.ExceptionInfo (run/run! handlers {} sample-plan)))
    (testing "and nothing after it runs"
      (is (= [:step/clean] (mapv first @log))))))

(deftest extra-handlers-are-harmless
  (testing "a handler set may cover kinds this plan does not use"
    (let [handlers (recorder (atom []) (conj all-kinds :step/publish :step/compile))]
      (is (run/runnable? handlers sample-plan))
      (is (= 4 (count (run/run! handlers {} sample-plan)))))))
