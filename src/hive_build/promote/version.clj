(ns hive-build.promote.version
  "Semantic versions as values: parse, render, order, bump. Pure."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [hive-build.schema :as s]))

(def ^:private digits #"\d+")

(defn parse
  "`text` as a SemVer, or nil when it is not MAJOR.MINOR.PATCH."
  [text]
  (when (string? text)
    (let [parts (str/split (str/trim text) #"\." 4)]
      (when (and (= 3 (count parts))
                 (every? #(re-matches digits %) parts))
        (let [[major minor patch] (map parse-long parts)]
          {:semver/major major :semver/minor minor :semver/patch patch})))))

(defn render
  "`semver` as MAJOR.MINOR.PATCH."
  [{:semver/keys [major minor patch]}]
  (str major "." minor "." patch))

(defn compare-versions
  "-1, 0 or 1 by major, then minor, then patch."
  [a b]
  (compare [(:semver/major a) (:semver/minor a) (:semver/patch a)]
           [(:semver/major b) (:semver/minor b) (:semver/patch b)]))

(defn bump
  "`semver` advanced at `level`.

   Components below `level` reset to zero, which is what makes the result
   strictly greater than its input under `compare-versions`."
  [semver level]
  (let [{:semver/keys [major minor patch]} semver]
    (case level
      :major {:semver/major (inc major) :semver/minor 0 :semver/patch 0}
      :minor {:semver/major major :semver/minor (inc minor) :semver/patch 0}
      :patch {:semver/major major :semver/minor minor :semver/patch (inc patch)}
      (throw (ex-info "level must be :major, :minor or :patch" {:level level})))))

(defn next-version
  "`text` bumped at `level`, as a string.

   Throws when `text` is not a semantic version: a release must never guess at
   its own coordinate."
  [text level]
  (if-let [current (parse text)]
    (render (bump current level))
    (throw (ex-info "VERSION is not MAJOR.MINOR.PATCH" {:version text}))))

(m/=> parse [:=> [:cat :any] [:maybe s/SemVer]])
(m/=> render [:=> [:cat s/SemVer] s/VersionString])
(m/=> compare-versions [:=> [:cat s/SemVer s/SemVer] [:int {:min -1 :max 1}]])
(m/=> bump [:=> [:cat s/SemVer s/BumpLevel] s/SemVer])
(m/=> next-version [:=> [:cat s/VersionString s/BumpLevel] s/VersionString])
