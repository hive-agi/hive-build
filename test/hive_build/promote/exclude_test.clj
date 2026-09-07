(ns hive-build.promote.exclude-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-schemas.test :as st]
            [hive-test.mutation :as mut]
            [hive-build.promote.exclude :as exclude]))

;; ── Value space ───────────────────────────────────────────────────────────

(def Path
  "Jar entry and exclude spellings that exercise every boundary: an exact
   file, a sibling sharing its prefix, a directory with and without the zip
   writer's trailing slash, an entry beneath it, a sibling directory sharing
   its prefix, and the blank spellings that must claim nothing."
  [:enum "hive_qdrant/addon.clj" "hive_qdrant/addon.cljc" "hive_qdrant/store.clj"
   "META-INF/hive-addons" "META-INF/hive-addons/" "/META-INF/hive-addons/"
   "META-INF/hive-addons/hive-qdrant.edn" "META-INF/hive-addons-extra/x.edn"
   "META-INF/MANIFEST.MF" "" "/"])

(defn- trim-slashes [s]
  (-> s (str/replace #"^/+" "") (str/replace #"/+$" "")))

(defn- claimed?
  "The specification: an exclude claims an entry when, slashes trimmed, it
   is that entry or a directory above it. A blank exclude claims nothing."
  [excludes entry]
  (let [e (trim-slashes entry)]
    (boolean (some (fn [x]
                     (let [x (trim-slashes x)]
                       (and (seq x)
                            (or (= x e) (str/starts-with? e (str x "/"))))))
                   excludes))))

;; ── Synthesized from the schemas ───────────────────────────────────────────

(st/deftrifecta-from-schema claims?
  hive-build.promote.exclude/claims?
  {:in [:cat [:vector Path] Path]
   :out :boolean
   :rel (fn [[excludes entry] out] (= out (claimed? excludes entry)))
   ;; A boolean output derives no mutants; the witnesses below carry that.
   :mutation false
   :num-tests 300})

(st/deftrifecta-from-schema violations
  hive-build.promote.exclude/violations
  {:in [:cat [:vector Path] [:vector Path]]
   :out [:vector :string]
   :rel (fn [[excludes entries] out]
          (= out (vec (sort (filter #(claimed? excludes %) entries)))))
   :mutation false
   :num-tests 300})

(st/deftrifecta-from-schema report
  hive-build.promote.exclude/report
  {:in [:vector [:enum "a/b.clj" "c/" "META-INF/x/y.edn"]]
   :out [:maybe :string]
   :rel (fn [in out]
          (if (empty? in)
            (nil? out)
            (and (string? out)
                 (str/includes? out (str (count in)))
                 (every? #(str/includes? out %) in))))
   :mutation false
   :num-tests 100})

;; ── Mutation: the relation above must catch each way claims? can be wrong ──

(def ^:private excludes
  ["hive_qdrant/addon.clj" "hive_qdrant/lifecycle.clj" "META-INF/hive-addons"])

(mut/deftest-mutations claims?-mutations
  hive-build.promote.exclude/claims?
  [["claims by bare prefix, so a sibling sharing the prefix is claimed"
    (fn [xs e] (boolean (some #(str/starts-with? e %) xs)))]
   ["claims by exact name only, so a directory claims nothing beneath it"
    (fn [xs e] (boolean (some #(= % e) xs)))]
   ["never normalizes, so the zip writer's trailing slash escapes"
    (fn [xs e] (boolean (some #(or (= % e) (str/starts-with? e (str % "/"))) xs)))]
   ["claims nothing"
    (fn [_ _] false)]
   ["claims everything"
    (fn [_ _] true)]]
  (fn []
    (is (exclude/claims? excludes "hive_qdrant/addon.clj"))
    (is (not (exclude/claims? excludes "hive_qdrant/addon.cljc")))
    (is (exclude/claims? excludes "META-INF/hive-addons/"))
    (is (exclude/claims? excludes "META-INF/hive-addons/hive-qdrant.edn"))
    (testing "slashes on the CONFIGURED side are trimmed too"
      (is (exclude/claims? ["META-INF/hive-addons/"] "META-INF/hive-addons"))
      (is (exclude/claims? ["/hive_qdrant/addon.clj"] "hive_qdrant/addon.clj")))
    (is (not (exclude/claims? excludes "META-INF/hive-addons-extra/x.edn")))
    (is (not (exclude/claims? excludes "META-INF/MANIFEST.MF")))))

(mut/deftest-mutations violations-mutations
  hive-build.promote.exclude/violations
  [["reports in the order given rather than sorted"
    (fn [xs es] (vec (filter #(exclude/claims? xs %) es)))]
   ["reports the excludes instead of the entries they claimed"
    (fn [xs _] (vec xs))]
   ["reports every entry"
    (fn [_ es] (vec (sort es)))]
   ["reports nothing"
    (fn [_ _] [])]]
  (fn []
    (is (= ["META-INF/hive-addons/" "META-INF/hive-addons/hive-qdrant.edn"
            "hive_qdrant/addon.clj"]
           (exclude/violations excludes
                               ["hive_qdrant/store.clj" "hive_qdrant/addon.clj"
                                "META-INF/hive-addons/hive-qdrant.edn"
                                "META-INF/hive-addons/" "META-INF/MANIFEST.MF"])))))

;; ── What a schema cannot state ────────────────────────────────────────────

(deftest a-blank-exclude-claims-nothing-rather-than-everything
  (testing "the failure mode that would empty a jar silently"
    (is (not (exclude/claims? [""] "anything")))
    (is (not (exclude/claims? ["/"] "anything")))
    (is (= [] (exclude/violations [""] ["a" "b/c"])))))

(deftest the-report-is-silent-when-there-is-nothing-to-say
  (is (nil? (exclude/report [])))
  (is (= "jar carries 1 entry that version.edn :jar-excludes claims: a/b.clj"
         (exclude/report ["a/b.clj"]))))
