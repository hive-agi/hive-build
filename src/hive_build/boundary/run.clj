(ns hive-build.boundary.run
  "Executes a Plan against a handler registry.

   Knows nothing about tools.build. A handler is (fn [ctx step] -> result)
   keyed by :step/kind, which is the seam the whole suite exercises: substitute
   recording handlers and an entire release runs with no filesystem, no
   network, and no jar."
  (:refer-clojure :exclude [run!])
  (:require [malli.core :as m]
            [hive-build.schema :as s]))

(defn missing-handlers
  "Step kinds in `plan` that `handlers` cannot execute, in plan order."
  [handlers plan]
  (into [] (comp (map :step/kind) (remove (set (keys handlers))) (distinct)) plan))

(defn runnable?
  "True when `handlers` covers every step of `plan`."
  [handlers plan]
  (empty? (missing-handlers handlers plan)))

(defn run!
  "Execute every step of `plan` in order against `handlers`, threading `ctx`.
   Returns the vector of step results.

   The plan is checked for coverage before the first effect. A release that
   ran half its steps and reported success is the failure this design exists
   to prevent, so an unhandled step stops it before anything happens rather
   than when it is reached."
  [handlers ctx plan]
  (when-let [missing (seq (missing-handlers handlers plan))]
    (throw (ex-info "no handler for step kind" {:missing missing
                                                :handled (sort (keys handlers))})))
  (mapv (fn [step] ((get handlers (:step/kind step)) ctx step)) plan))

(m/=> missing-handlers [:=> [:cat [:map-of :keyword fn?] s/Plan] [:vector :keyword]])
(m/=> runnable? [:=> [:cat [:map-of :keyword fn?] s/Plan] :boolean])
