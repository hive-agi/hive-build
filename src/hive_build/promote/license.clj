(ns hive-build.promote.license
  "Licence agreement between ./LICENSE, version.edn :license and the SPDX
   headers in the source tree, as an open rule chain. Pure.

   A rule returns nil when satisfied, else a message. Unlike an access
   decision, every rule runs: a licence report that stopped at the first
   problem would hide the rest, and a published pom can never be retracted."
  (:require [malli.core :as m]
            [hive-build.schema :as s]))

(defprotocol ILicenseRule
  (rule-id [this] "Keyword identifying this rule.")
  (problem [this facts] "nil when satisfied, else a message string."))

(def ^:private license-file-present
  (reify ILicenseRule
    (rule-id [_] :license-file-present)
    (problem [_ {:license.facts/keys [file?]}]
      (when-not file? "no ./LICENSE file"))))

(def ^:private license-declared
  (reify ILicenseRule
    (rule-id [_] :license-declared)
    (problem [_ {:license.facts/keys [declared]}]
      (when (nil? declared) "version.edn has no :license"))))

(def ^:private spdx-consistent
  (reify ILicenseRule
    (rule-id [_] :spdx-consistent)
    (problem [_ {:license.facts/keys [spdx-ids]}]
      (when (> (count spdx-ids) 1)
        (str "conflicting SPDX headers in src: " (sort spdx-ids))))))

(def ^:private spdx-matches-declaration
  (reify ILicenseRule
    (rule-id [_] :spdx-matches-declaration)
    (problem [_ {:license.facts/keys [declared spdx-ids]}]
      (when (and declared (= 1 (count spdx-ids)) (not (contains? spdx-ids declared)))
        (str "version.edn declares " declared
             " but src headers say " (first spdx-ids))))))

(def default-rules
  "Every rule is evaluated; order fixes only the order of the messages."
  [license-file-present
   license-declared
   spdx-consistent
   spdx-matches-declaration])

(defn report
  "Findings for `facts`. `:report/ok?` is true exactly when there are none."
  ([facts] (report default-rules facts))
  ([rules facts]
   (let [problems (into [] (keep #(problem % facts)) rules)]
     {:report/ok? (empty? problems)
      :report/problems problems})))

(m/=> report [:function
              [:=> [:cat s/LicenseFacts] s/LicenseReport]
              [:=> [:cat [:sequential :any] s/LicenseFacts] s/LicenseReport]])
