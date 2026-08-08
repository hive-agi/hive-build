(ns hive-build.promote.naming
  "Names derived from a coordinate: the Maven symbol, artifact paths, the
   registry URL a publish check probes, and the namespace/path correspondence
   AOT compilation depends on. Pure."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [hive-build.schema :as s]))

(defn coordinate
  "The Coordinate named by tools.build `lib` symbol and `version`."
  [lib version]
  {:coordinate/group-id (or (namespace lib) (name lib))
   :coordinate/artifact-id (name lib)
   :coordinate/version version})

(defn lib-symbol
  "`coordinate` as the group/artifact symbol tools.build expects."
  [{:coordinate/keys [group-id artifact-id]}]
  (symbol group-id artifact-id))

(defn coordinate-label
  "`coordinate` as it is printed in build output."
  [{:coordinate/keys [group-id artifact-id]}]
  (str group-id "/" artifact-id))

(defn jar-file
  "Path of the jar `coordinate` builds to under `target-dir`."
  [target-dir {:coordinate/keys [artifact-id version]}]
  (str target-dir "/" artifact-id "-" version ".jar"))

(defn pom-url
  "Absolute URL of `coordinate`'s pom in the Maven repository at `repo-url`.

   Both registries are immutable, so the existence of this document is what
   decides whether a deploy is a re-run or a new release."
  [repo-url {:coordinate/keys [group-id artifact-id version]}]
  (str (str/replace repo-url #"/+$" "")
       "/" (str/replace group-id "." "/")
       "/" artifact-id
       "/" version
       "/" artifact-id "-" version ".pom"))

(defn ns->path
  "`ns-sym` as the classpath prefix its compiled classes live under."
  [ns-sym]
  (-> (str ns-sym) (str/replace "-" "_") (str/replace "." "/")))

(defn path->ns
  "`path` back to the namespace name it was derived from.

   Inverse of `ns->path` for every namespace whose name contains no
   underscore, which is every namespace Clojure's own file convention can
   express."
  [path]
  (-> (str path) (str/replace "/" ".") (str/replace "_" "-") symbol))

(m/=> own-class? [:=> [:cat [:sequential :string] :string] :boolean])

(defn own-class?
  "True when relative path `rel` names a compiled class belonging to one of
   `prefixes`.

   The AOT scratch directory also holds classes for preloaded host namespaces
   compiled in the same JVM. Only paths under this project's own namespace
   prefixes may enter the published jar."
  [prefixes rel]
  (boolean (and (str/ends-with? rel ".class")
                (some #(str/starts-with? rel %) prefixes))))

(m/=> coordinate [:=> [:cat :symbol s/VersionString] s/Coordinate])
(m/=> lib-symbol [:=> [:cat s/Coordinate] :symbol])
(m/=> coordinate-label [:=> [:cat s/Coordinate] [:string {:min 1}]])
(m/=> jar-file [:=> [:cat [:string {:min 1}] s/Coordinate] [:string {:min 1}]])
(m/=> pom-url [:=> [:cat [:string {:min 1}] s/Coordinate] [:string {:min 1}]])
(m/=> ns->path [:=> [:cat s/NsSymbol] [:string {:min 1}]])
(m/=> path->ns [:=> [:cat [:string {:min 1}]] :symbol])
