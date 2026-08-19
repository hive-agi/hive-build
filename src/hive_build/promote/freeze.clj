(ns hive-build.promote.freeze
  "The freeze policy, evaluated. Pure.

   A contract library's whole value is that consumers can pin it and forget it.
   Two things destroy that: a change they cannot survive, and a cadence that
   makes them re-pin constantly. This namespace decides both from data.

   Policy shape (./freeze-policy.edn in the repo being released):

     {:policy/version 1
      :cadence {:mode :scheduled | :on-merge
                :min-days-between-releases 90
                :exception-marker \"release-exception:\"}
      :compatibility {:additive-only? true
                      :break-marker \"BREAKING-CHANGE:\"}}

   Contract: `evaluate` never throws and never consults the world — every
   observation (the diff, the age, the commit message) is passed in."
  (:require [clojure.string :as str]))

(def default-policy
  "The policy a repo gets when it ships no freeze-policy.edn: nothing is
   enforced. Absence of a policy must not silently mean absence of releases."
  {:policy/version 1
   :cadence {:mode :on-merge}
   :compatibility {:additive-only? false}})

(defn- marker-present?
  [message marker]
  (boolean (and (string? message) (string? marker)
                (str/includes? message marker))))

(defn cadence-violation
  "A violation map when this release breaks the cadence rule, else nil.

   `days-since-last` nil means there is no previous release — the first release
   is always allowed."
  [{:keys [cadence]} {:keys [days-since-last commit-message]}]
  (let [{:keys [mode min-days-between-releases exception-marker]} cadence]
    (when (and (= :scheduled mode)
               (number? days-since-last)
               (number? min-days-between-releases)
               (< days-since-last min-days-between-releases)
               (not (marker-present? commit-message exception-marker)))
      {:violation :freeze/cadence
       :days-since-last days-since-last
       :min-days min-days-between-releases
       :diagnosis (str "the last release was " days-since-last " day(s) ago and this "
                       "repo releases on a " min-days-between-releases "-day cadence. "
                       "Every consumer re-pins on a change that was supposed to be "
                       "stable. Add \"" exception-marker "\" to the commit message to "
                       "release anyway, and say why.")})))

(defn compatibility-violation
  "A violation map when this release breaks consumers, else nil."
  [{:keys [compatibility]} {:keys [diff breaking? descriptions commit-message]}]
  (let [{:keys [additive-only? break-marker]} compatibility]
    (when (and additive-only?
               breaking?
               (not (marker-present? commit-message break-marker)))
      {:violation :freeze/not-additive
       :changes (vec descriptions)
       :counts (into {} (for [k [:removed :arities-lost :methods-lost :kind-changed]]
                          [k (count (get diff k))]))
       :diagnosis (str "this release removes or narrows a published name. A "
                       "contract that churns is the worst kind of hub: every "
                       "consumer re-pins on a change that was supposed to be "
                       "stable. Make the change additive, or mark the commit "
                       "\"" break-marker "\" and cut a MAJOR.")})))

(defn evaluate
  "The freeze verdict for one release attempt.

   `observations`: {:diff :breaking? :descriptions :days-since-last :commit-message}
   Returns {:ok? bool :violations [...] :policy policy}."
  [policy observations]
  (let [p (merge default-policy policy)
        vs (remove nil? [(cadence-violation p observations)
                         (compatibility-violation p observations)])]
    {:ok? (empty? vs)
     :violations (vec vs)
     :policy p}))

(defn report-lines
  "Human-readable verdict, for CI output."
  [{:keys [ok? violations]}]
  (if ok?
    ["freeze-check: OK"]
    (into ["freeze-check: REFUSED"]
          (mapcat (fn [{:keys [violation diagnosis changes]}]
                    (concat [(str "  " (name violation))
                             (str "    " diagnosis)]
                            (map #(str "    - " %) (or changes []))))
                  violations))))
