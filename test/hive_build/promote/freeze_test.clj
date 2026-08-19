(ns hive-build.promote.freeze-test
  "The freeze gate, proven in both directions: silent on an additive release,
   refusing on the two shapes that cost consumers a re-pin."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-build.boundary.freeze :as gitf]
            [hive-build.promote.api-surface :as surface]
            [hive-build.promote.freeze :as freeze]))

(def ^:private strict
  {:policy/version 1
   :cadence {:mode :scheduled
             :min-days-between-releases 90
             :exception-marker "release-exception:"}
   :compatibility {:additive-only? true
                   :break-marker "BREAKING-CHANGE:"}})

(defn- surface-of [text] (surface/surface (gitf/read-forms text)))

;; =============================================================================
;; Reading a surface
;; =============================================================================

(deftest public-names-are-collected-private-ones-are-not
  (let [s (surface-of "(ns a.b) (defn pub [x] x) (defn- priv [x] x) (def ^:private hidden 1) (def shown 2)")]
    (is (contains? s "a.b/pub"))
    (is (contains? s "a.b/shown"))
    (is (not (contains? s "a.b/priv")) "defn- leaked into the frozen surface")
    (is (not (contains? s "a.b/hidden")) "^:private leaked into the frozen surface")))

(deftest arities-are-part-of-the-contract
  (let [s (surface-of "(ns a.b) (defn f ([x] x) ([x y] y)) (defn g [& more] more)")]
    (is (= #{1 2} (:arities (get s "a.b/f"))))
    (is (= #{:variadic} (:arities (get s "a.b/g"))))))

(deftest protocols-and-their-methods-are-read-through-the-reload-guard
  (testing "the contract repos wrap defprotocol in defonce+do so reloads do not
            mint fresh protocol objects — the reader must see through it"
    (let [s (surface-of (str "(ns a.b) (defonce -guard (do (defprotocol P "
                             "(m [this] [this x] \"doc\") (n [this] \"doc\"))))"))]
      (is (= :defprotocol (:kind (get s "a.b/P"))))
      (is (= {"m" #{1 2} "n" #{1}} (:methods (get s "a.b/P")))))))

(deftest an-unreadable-file-yields-no-surface-not-an-exception
  (is (= [] (gitf/read-forms "(ns a.b) (defn oops [")))
  (is (= {} (surface-of "(defn no-ns [x] x)"))))

;; =============================================================================
;; The diff
;; =============================================================================

(deftest adding-is-never-breaking
  (let [old (surface-of "(ns a.b) (defn f [x] x)")
        new (surface-of "(ns a.b) (defn f [x] x) (defn g [y] y)")
        d   (surface/diff old new)]
    (is (= ["a.b/g"] (:added d)))
    (is (not (surface/breaking? d)))))

(deftest removing-a-public-name-is-breaking
  (let [d (surface/diff (surface-of "(ns a.b) (defn f [x] x)") (surface-of "(ns a.b)"))]
    (is (= ["a.b/f"] (:removed d)))
    (is (surface/breaking? d))
    (is (= ["REMOVED  a.b/f"] (surface/describe d)))))

(deftest dropping-an-arity-is-breaking
  (let [d (surface/diff (surface-of "(ns a.b) (defn f ([x] x) ([x y] y))")
                        (surface-of "(ns a.b) (defn f [x] x)"))]
    (is (surface/breaking? d))
    (is (= [{:qn "a.b/f" :lost [2]}] (:arities-lost d)))))

(deftest making-a-name-private-reads-as-removal
  (testing "a consumer cannot call it either way, so the gate must not be
            fooled by the var still existing"
    (let [d (surface/diff (surface-of "(ns a.b) (defn f [x] x)")
                          (surface-of "(ns a.b) (defn- f [x] x)"))]
      (is (= ["a.b/f"] (:removed d))))))

(deftest narrowing-a-protocol-method-is-breaking
  (let [old (surface-of "(ns a.b) (defprotocol P (m [this] [this x] \"d\"))")
        new (surface-of "(ns a.b) (defprotocol P (m [this] \"d\"))")
        d   (surface/diff old new)]
    (is (surface/breaking? d))
    (is (= [{:qn "a.b/P" :method "m" :change :arities-lost :lost [2]}]
           (:methods-lost d)))))

(deftest removing-a-protocol-method-is-breaking
  (let [d (surface/diff (surface-of "(ns a.b) (defprotocol P (m [this] \"d\") (n [this] \"d\"))")
                        (surface-of "(ns a.b) (defprotocol P (m [this] \"d\"))"))]
    (is (= [{:qn "a.b/P" :method "n" :change :removed}] (:methods-lost d)))))

;; =============================================================================
;; The verdict
;; =============================================================================

(deftest an-additive-release-inside-the-cadence-passes
  (let [v (freeze/evaluate strict {:diff {} :breaking? false :descriptions []
                                   :days-since-last 120 :commit-message "feat: add a port"})]
    (is (:ok? v))
    (is (= ["freeze-check: OK"] (freeze/report-lines v)))))

(deftest a-breaking-release-is-refused
  (let [v (freeze/evaluate strict {:diff {:removed ["a.b/f"]} :breaking? true
                                   :descriptions ["REMOVED  a.b/f"]
                                   :days-since-last 120 :commit-message "refactor: tidy"})]
    (is (false? (:ok? v)))
    (is (= [:freeze/not-additive] (mapv :violation (:violations v))))))

(deftest a-breaking-release-passes-when-the-commit-declares-it
  (let [v (freeze/evaluate strict {:diff {:removed ["a.b/f"]} :breaking? true
                                   :descriptions ["REMOVED  a.b/f"]
                                   :days-since-last 120
                                   :commit-message "refactor!: drop f\n\nBREAKING-CHANGE: f had no consumers"})]
    (is (:ok? v) "the declared-break escape hatch did not open")))

(deftest releasing-inside-the-cadence-window-is-refused
  (let [v (freeze/evaluate strict {:diff {} :breaking? false :descriptions []
                                   :days-since-last 3 :commit-message "feat: another port"})]
    (is (false? (:ok? v)))
    (is (= [:freeze/cadence] (mapv :violation (:violations v))))
    (is (= 3 (:days-since-last (first (:violations v)))))))

(deftest the-cadence-exception-must-be-written-in-the-commit
  (let [v (freeze/evaluate strict {:diff {} :breaking? false :descriptions []
                                   :days-since-last 3
                                   :commit-message "fix: NPE in the port\n\nrelease-exception: consumers are broken today"})]
    (is (:ok? v))))

(deftest the-first-release-is-always-allowed
  (testing "no previous tag means no baseline and no cadence to violate"
    (let [v (freeze/evaluate strict {:diff {} :breaking? false :descriptions []
                                     :days-since-last nil :commit-message "chore: first release"})]
      (is (:ok? v)))))

(deftest both-violations-are-reported-together
  (let [v (freeze/evaluate strict {:diff {:removed ["a.b/f"]} :breaking? true
                                   :descriptions ["REMOVED  a.b/f"]
                                   :days-since-last 1 :commit-message "wip"})]
    (is (= #{:freeze/cadence :freeze/not-additive} (set (map :violation (:violations v)))))
    (is (some #(re-find #"REMOVED" %) (freeze/report-lines v))
        "the report must name the change, not just the rule")))

(deftest no-policy-file-enforces-nothing
  (testing "a repo that has not opted in must keep releasing as before"
    (let [v (freeze/evaluate freeze/default-policy
                             {:diff {:removed ["a.b/f"]} :breaking? true
                              :descriptions ["REMOVED  a.b/f"]
                              :days-since-last 0 :commit-message ""})]
      (is (:ok? v)))))
