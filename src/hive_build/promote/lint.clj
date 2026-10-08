(ns hive-build.promote.lint
  "clj-kondo invocations as values. Pure.

   Constructing the argument vector is where a lint task silently stops
   failing, so it is stated here and asserted rather than inlined at the
   process call."
  (:require [clojure.string :as str]
            [malli.core :as m]))

(defn test-roots
  "The test roots among `dir-names`, the repo's top-level directory names:
   every name starting with \"test\" (test, test-swarm, test-integration ...),
   sorted, without duplicates. The universe comes from the tree, never from a
   literal, so a repo that grows a second test root is not silently
   half-linted."
  [dir-names]
  (vec (sort (distinct (filter #(str/starts-with? % "test") dir-names)))))

(defn lint-candidates
  "Default lint targets for `src-dirs`: the source roots minus resources, plus
   test/. The caller drops the ones that do not exist on disk.

   The two-argument form takes the repo's top-level `dir-names` and lints
   every test root among them (see `test-roots`) instead of test/ alone."
  ([src-dirs]
   (conj (vec (remove #{"resources"} src-dirs)) "test"))
  ([src-dirs dir-names]
   (let [srcs (vec (remove #{"resources"} src-dirs))]
     (into srcs (remove (set srcs)) (or (seq (test-roots dir-names)) ["test"])))))

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

(m/=> test-roots [:=> [:cat [:sequential [:string {:min 1}]]] [:vector [:string {:min 1}]]])
(m/=> lint-candidates
      [:function
       [:=> [:cat [:vector [:string {:min 1}]]] [:vector [:string {:min 1}]]]
       [:=> [:cat [:vector [:string {:min 1}]] [:sequential [:string {:min 1}]]]
        [:vector [:string {:min 1}]]]])
(m/=> classpath [:=> [:cat [:sequential [:maybe :string]]] :string])
(m/=> sync-command [:=> [:cat :string] [:vector [:string {:min 1}]]])
(m/=> lint-command
      [:=> [:cat [:vector [:string {:min 1}]] [:maybe :keyword]]
       [:vector [:string {:min 1}]]])
