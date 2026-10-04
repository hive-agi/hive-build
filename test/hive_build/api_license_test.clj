(ns hive-build.api-license-test
  "verify-license as CI runs it: strict mode turns an inconsistent licence
   into a failed step. The verdict takes the project and the report as
   values, so nothing is redefined."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-build.api :as api]
            [hive-build.promote.license :as license]))

(def project
  {:project/coordinate {:coordinate/group-id "io.github.hive-agi"
                        :coordinate/artifact-id "hive-thing"}
   :project/license {:license/name "MIT"}})

(def ok-report (license/report {:license.facts/declared "MIT"
                                :license.facts/file? true
                                :license.facts/spdx-ids #{"MIT"}}))

(def drifted-report (license/report {:license.facts/declared "LicenseRef-Proprietary"
                                     :license.facts/file? true
                                     :license.facts/spdx-ids #{"MIT"}}))

(deftest an-agreeing-licence-passes-in-both-modes
  (is (= ok-report (api/license-verdict project ok-report false)))
  (is (= ok-report (api/license-verdict project ok-report true))))

(deftest advisory-mode-warns-and-returns
  (let [out (with-out-str
              (is (= drifted-report (api/license-verdict project drifted-report false))))]
    (is (str/includes? out "WARNING: license inconsistency in io.github.hive-agi/hive-thing"))))

(deftest strict-mode-fails-loudly
  (is (false? (:report/ok? drifted-report)) "fixture really is inconsistent")
  (let [e (try (with-out-str (api/license-verdict project drifted-report true)) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (some? e) "strict throws, so `clojure -T:build verify-license :strict true` exits non-zero")
    (testing "the failure names the artifact and carries the report"
      (is (str/includes? (ex-message e) "io.github.hive-agi/hive-thing"))
      (is (= drifted-report (:license/report (ex-data e)))))))

(deftest a-missing-licence-file-fails-strict
  (let [r (license/report {:license.facts/declared "MIT"
                           :license.facts/file? false
                           :license.facts/spdx-ids #{}})]
    (is (thrown? clojure.lang.ExceptionInfo
                 (with-out-str (api/license-verdict project r true))))))
