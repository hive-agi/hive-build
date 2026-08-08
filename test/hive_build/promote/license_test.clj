(ns hive-build.promote.license-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.test :as st]
            [hive-build.promote.license :as license]
            [hive-build.schema :as s]))

(def clean-facts
  {:license.facts/declared "MIT"
   :license.facts/file? true
   :license.facts/spdx-ids #{"MIT"}})

;; ── Synthesized from the schemas ───────────────────────────────────────────

(st/deftrifecta-from-schema report
  hive-build.promote.license/report
  {:in s/LicenseFacts
   :out s/LicenseReport
   :rel (fn [_in out] (= (:report/ok? out) (empty? (:report/problems out))))
   :num-tests 200})

;; ── The rule table ────────────────────────────────────────────────────────

(deftest agreement-is-reported-as-ok
  (is (= {:report/ok? true :report/problems []} (license/report clean-facts))))

(deftest each-rule-fires-on-its-own-fact
  (testing "a missing LICENSE file"
    (is (= ["no ./LICENSE file"]
           (:report/problems (license/report (assoc clean-facts :license.facts/file? false))))))
  (testing "an undeclared licence"
    (is (= ["version.edn has no :license"]
           (:report/problems (license/report (assoc clean-facts
                                                   :license.facts/declared nil))))))
  (testing "sources that disagree with each other"
    (let [problems (:report/problems
                    (license/report (assoc clean-facts
                                           :license.facts/spdx-ids #{"MIT" "AGPL-3.0"})))]
      (is (= 1 (count problems)))
      (is (re-find #"conflicting SPDX headers" (first problems)))))
  (testing "sources that disagree with version.edn"
    (let [problems (:report/problems
                    (license/report (assoc clean-facts :license.facts/spdx-ids #{"AGPL-3.0"})))]
      (is (= 1 (count problems)))
      (is (re-find #"declares MIT but src headers say AGPL-3.0" (first problems))))))

(deftest every-rule-runs
  (testing "a licence report that stopped at the first problem would hide the rest,
            and a published pom can never be retracted"
    (let [problems (:report/problems
                    (license/report {:license.facts/declared nil
                                     :license.facts/file? false
                                     :license.facts/spdx-ids #{"MIT" "AGPL-3.0"}}))]
      (is (= 3 (count problems)))
      (is (= ["no ./LICENSE file" "version.edn has no :license"] (take 2 problems))))))

(deftest an-undeclared-licence-is-not-blamed-for-disagreeing
  (testing "the mismatch rule needs something to mismatch against"
    (is (= ["version.edn has no :license"]
           (:report/problems (license/report {:license.facts/declared nil
                                              :license.facts/file? true
                                              :license.facts/spdx-ids #{"MIT"}}))))))

(deftest sources-without-headers-are-not-a-problem
  (testing "SPDX headers are optional; only disagreement is a finding"
    (is (:report/ok? (license/report (assoc clean-facts :license.facts/spdx-ids #{}))))))

;; ── Open for extension ────────────────────────────────────────────────────

(deftest the-chain-extends-without-editing-the-fold
  (let [copyright-year
        (reify license/ILicenseRule
          (rule-id [_] :copyright-year)
          (problem [_ facts]
            (when-not (:license.facts/file? facts) "no year to check")))
        rules (conj license/default-rules copyright-year)]
    (testing "a new rule adds its finding and changes nothing else"
      (is (= (:report/problems (license/report clean-facts))
             (:report/problems (license/report rules clean-facts))))
      (is (= 2 (count (:report/problems
                       (license/report rules (assoc clean-facts
                                                    :license.facts/file? false)))))))))

(deftest every-default-rule-has-a-distinct-id
  (let [ids (map license/rule-id license/default-rules)]
    (is (= (count ids) (count (distinct ids))))
    (is (every? keyword? ids))))
