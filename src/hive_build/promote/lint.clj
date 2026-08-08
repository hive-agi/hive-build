(ns hive-build.promote.lint
  "clj-kondo invocations as values. Pure.

   Constructing the argument vector is where a lint task silently stops
   failing, so it is stated here and asserted rather than inlined at the
   process call."
  (:require [clojure.string :as str]
            [malli.core :as m]))

(defn lint-candidates
  "Default lint targets for `src-dirs`: the source roots minus resources, plus
   test/. The caller drops the ones that do not exist on disk."
  [src-dirs]
  (conj (vec (remove #{"resources"} src-dirs)) "test"))

(defn classpath
  "`parts` joined for clj-kondo's --lint, dropping the absent ones."
  [parts]
  (str/join java.io.File/pathSeparator (remove str/blank? parts)))

(defn sync-command
  "Argument vector that copies dependency-exported configs into
   ./.clj-kondo/imports/, where clj-kondo loads them automatically."
  [cp]
  ["clj-kondo" "--lint" cp "--dependencies" "--parallel" "--copy-configs"])

(defn lint-command
  "Argument vector that lints `paths`. A nil `fail-level` reports without
   failing the build."
  [paths fail-level]
  (cond-> (into ["clj-kondo" "--lint"] paths)
    fail-level (into ["--fail-level" (name fail-level)])))

(m/=> lint-candidates [:=> [:cat [:vector [:string {:min 1}]]] [:vector [:string {:min 1}]]])
(m/=> classpath [:=> [:cat [:sequential [:maybe :string]]] :string])
(m/=> sync-command [:=> [:cat :string] [:vector [:string {:min 1}]]])
(m/=> lint-command
      [:=> [:cat [:vector [:string {:min 1}]] [:maybe :keyword]]
       [:vector [:string {:min 1}]]])
