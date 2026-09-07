(ns hive-build.promote.exclude
  "Jar-entry exclusion: which entries a version.edn :jar-excludes list claims,
   and which entries of a built jar violate it. Pure."
  (:require [clojure.string :as str]
            [malli.core :as m]))

(defn normalize
  "`path` without leading or trailing slashes, so a directory entry the zip
   writer spelled `META-INF/hive-addons/` and the config's `META-INF/hive-addons`
   name the same thing."
  [path]
  (-> path (str/replace #"^/+" "") (str/replace #"/+$" "")))

(defn claims?
  "True when one of `excludes` names `entry-name` exactly or names a directory
   above it. `hive_qdrant/addon.clj` claims that file only; `META-INF/hive-addons`
   claims the directory entry and everything beneath it."
  [excludes entry-name]
  (let [entry (normalize entry-name)]
    (boolean (some (fn [x]
                     (let [x (normalize x)]
                       (and (not (str/blank? x))
                            (or (= x entry)
                                (str/starts-with? entry (str x "/"))))))
                   excludes))))

(defn violations
  "The `entry-names` that `excludes` claims, sorted."
  [excludes entry-names]
  (into [] (filter #(claims? excludes %)) (sort entry-names)))

(defn report
  "A message naming `violations`, or nil when there are none."
  [violations]
  (when (seq violations)
    (str "jar carries " (count violations) " entr"
         (if (= 1 (count violations)) "y" "ies")
         " that version.edn :jar-excludes claims: "
         (str/join ", " violations))))

(m/=> normalize [:=> [:cat :string] :string])
(m/=> claims? [:=> [:cat [:sequential :string] :string] :boolean])
(m/=> violations [:=> [:cat [:sequential :string] [:sequential :string]] [:vector :string]])
(m/=> report [:=> [:cat [:sequential :string]] [:maybe :string]])
