(ns hive-build.promote.manifest
  "Addon manifests as values. Pure.

   A hive addon ships resources/META-INF/hive-addons/<id>.edn describing
   itself. The version in that file is the one an addon store reads, so it is
   derived from the coordinate being built rather than hand-maintained."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [malli.core :as m]
            [hive-build.schema :as s]))

(def manifest-dir
  "Classpath directory a hive addon manifest is discovered under."
  "META-INF/hive-addons")

(defn manifest?
  "True when `path` is an addon manifest: an .edn file under META-INF/hive-addons."
  [path]
  (let [p (str/replace (str path) "\\" "/")]
    (boolean (and (str/ends-with? p ".edn")
                  (str/includes? p (str manifest-dir "/"))))))

(defn manifests
  "The addon manifests among `paths`, in the order given."
  [paths]
  (into [] (filter manifest?) paths))

(defn stamp
  "`text` (an addon manifest) with :addon/version set to `version`.

   Returns nil when the text does not read as a map, so a resource that merely
   sits in the manifest directory is left alone instead of being replaced by
   something this function invented."
  [text version]
  (let [value (try (edn/read-string text) (catch Exception _ nil))]
    (when (map? value)
      (str (pr-str (assoc value :addon/version version)) "\n"))))

(m/=> manifest? [:=> [:cat :any] :boolean])
(m/=> manifests [:=> [:cat [:sequential :any]] [:vector :any]])
(m/=> stamp [:=> [:cat :string s/VersionString] [:maybe :string]])
