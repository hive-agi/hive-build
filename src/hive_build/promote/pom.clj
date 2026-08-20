(ns hive-build.promote.pom
  "The parts of a pom hive-build decides rather than delegates: the declared
   licence, the dependency set, the source roots, the SCM tag and the
   repository set. Pure."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [hive-build.schema :as s]))

(def undeclared
  "What a pom says when version.edn declared no licence. Deliberately not a
   real licence name: a package with no :license must not silently inherit
   someone else's terms, and a published pom can never be retracted."
  "UNDECLARED")

(defn pom-data
  "The :pom-data fragment declaring `license`."
  [license]
  [[:licenses
    [:license
     [:name (get license :license/name undeclared)]
     [:url (get license :license/url "")]]]])

(defn prune-deps
  "`deps` without `exclude` — host-integration libs that are on the compile
   classpath but must not be declared as requirements of the published
   artifact.

   Contract: the result is a submap of `deps` and shares no key with
   `exclude`."
  [deps exclude]
  (apply dissoc deps exclude))

(def ^:private repositories-re
  #"(?s)[ \t]*<repositories>.*?</repositories>[ \t]*\r?\n?|[ \t]*<repositories[ \t]*/>[ \t]*\r?\n?")

(defn without-repositories
  "`pom-xml` with every <repositories> element removed.

   Contract: the result contains no \"<repositories\" substring, and equals the
   input when it declared none."
  [pom-xml]
  (str/replace pom-xml repositories-re ""))

(defn pom-src-dirs
  "`src-dirs` as the pom should list them: resources are packaged but are not
   source roots."
  [src-dirs]
  (vec (remove #{"resources"} src-dirs)))

(defn scm
  "The :scm map for `scm-url` at commit `sha`."
  [scm-url sha]
  {:url scm-url :tag sha})

(m/=> pom-data [:=> [:cat [:maybe s/License]] [:vector :any]])
(m/=> prune-deps [:=> [:cat [:map-of :symbol :any] [:set :symbol]] [:map-of :symbol :any]])
(m/=> pom-src-dirs [:=> [:cat [:vector [:string {:min 1}]]] [:vector [:string {:min 1}]]])
(m/=> scm [:=> [:cat [:maybe :string] [:maybe :string]] :map])
(m/=> without-repositories [:=> [:cat :string] :string])
