(ns hive-build.schema
  "Malli value objects for the release domain.

   Every hive-build contract is stated over these schemas, and the property and
   mutation facets of the suite are synthesized from them. This namespace has
   no dependencies beyond clojure.string: schemas are plain data.

   Generation: schemas whose validity is a regex carry an explicit
   `:gen/schema` + `:gen/fmap` so a generator exists without test.chuck."
  (:require [clojure.string :as str]))

;; ── Names, versions, coordinates ───────────────────────────────────────────

(def VersionNumber
  "One component of a semantic version."
  [:int {:min 0 :max 999999}])

(def name-token-chars
  "The characters a NameToken may contain after its leading letter."
  ["a" "b" "z" "0" "9" "-" "_" "."])

(def NameToken
  "A Maven-safe identifier segment: a leading letter, then letters, digits,
   dot, dash or underscore."
  [:and {:gen/schema [:vector {:min 0 :max 8} (into [:enum] name-token-chars)]
         :gen/fmap   (fn [cs] (apply str "h" cs))}
   :string
   [:re #"^[a-zA-Z][a-zA-Z0-9_.-]*$"]])

(def VersionString
  "MAJOR.MINOR.PATCH."
  [:and {:gen/schema [:tuple VersionNumber VersionNumber VersionNumber]
         :gen/fmap   (fn [[major minor patch]] (str major "." minor "." patch))}
   :string
   [:re #"^\d+\.\d+\.\d+$"]])

(def SemVer
  "A semantic version taken apart."
  [:map {:closed true}
   [:semver/major VersionNumber]
   [:semver/minor VersionNumber]
   [:semver/patch VersionNumber]])

(def BumpLevel
  [:enum :major :minor :patch])

(def Coordinate
  "What identifies an artifact in a Maven registry."
  [:map {:closed true}
   [:coordinate/group-id NameToken]
   [:coordinate/artifact-id NameToken]
   [:coordinate/version VersionString]])

(def LibSymbol
  "A Maven coordinate as tools.build spells it: group/artifact."
  [:and {:gen/schema [:tuple NameToken NameToken]
         :gen/fmap   (fn [[group artifact]] (symbol group artifact))}
   :symbol
   [:fn {:error/message "must be a qualified group/artifact symbol"}
    qualified-symbol?]])

(def NsSymbol
  "A Clojure namespace name."
  [:and {:gen/schema [:vector {:min 1 :max 4}
                      [:enum "a" "b" "core" "impl" "x-y" "zz"]]
         :gen/fmap   (fn [segments] (symbol (str/join "." segments)))}
   :symbol])

;; ── Publishing ─────────────────────────────────────────────────────────────

(def ArtifactKind
  "Which jar a target receives: copied sources, or this lib's own .class files
   with no sources at all."
  [:enum :artifact/source :artifact/aot])

(def Target
  "A publish destination, as data. Supporting a new registry is a `register!`
   of one of these and nothing else — credentials are named, never hard-coded."
  [:map {:closed true}
   [:target/id :keyword]
   [:target/artifact-kind ArtifactKind]
   [:target/publishes? :boolean]
   [:target/repo-url [:maybe [:string {:min 1}]]]
   [:target/repo-url-env [:maybe [:string {:min 1}]]]
   [:target/repository-name [:maybe [:string {:min 1}]]]
   [:target/username-env [:maybe [:string {:min 1}]]]
   [:target/password-env [:maybe [:string {:min 1}]]]])

(def License
  [:map {:closed true}
   [:license/name [:string {:min 1}]]
   [:license/url :string]])

(def Project
  "Everything a release needs to know about the repository being released,
   with every default already applied."
  [:map {:closed true}
   [:project/coordinate Coordinate]
   [:project/src-dirs [:vector [:string {:min 1}]]]
   [:project/target-dir [:string {:min 1}]]
   [:project/class-dir [:string {:min 1}]]
   [:project/scratch-dir [:string {:min 1}]]
   [:project/jar-file [:string {:min 1}]]
   [:project/target-id :keyword]
   [:project/license [:maybe License]]
   [:project/scm-url [:maybe :string]]
   [:project/elide-meta [:vector :keyword]]
   [:project/pom-exclude-deps [:set :symbol]]
   [:project/package-protocols [:vector LibSymbol]]
   [:project/aot-java-opts [:vector :string]]])

;; ── Facts ──────────────────────────────────────────────────────────────────

(def Facts
  "Everything read from the filesystem or the network that a plan depends on.
   Collected once at the boundary so planning stays pure and total."
  [:map {:closed true}
   [:facts/source-roots [:vector [:string {:min 1}]]]
   [:facts/resource-roots [:vector [:string {:min 1}]]]
   [:facts/namespaces [:vector NsSymbol]]
   [:facts/preload [:vector NsSymbol]]
   [:facts/published? :boolean]])

(def LicenseFacts
  [:map {:closed true}
   [:license.facts/declared [:maybe [:string {:min 1}]]]
   [:license.facts/file? :boolean]
   [:license.facts/spdx-ids [:set [:string {:min 1}]]]])

(def LicenseReport
  [:map {:closed true}
   [:report/ok? :boolean]
   [:report/problems [:vector [:string {:min 1}]]]])

;; ── Plan ───────────────────────────────────────────────────────────────────

(def Task
  [:enum :task/clean :task/jar :task/jar-aot :task/install :task/deploy])

(def Step
  "One executable unit of a release. A step names an effect and carries every
   argument that effect needs beyond the Project itself."
  [:multi {:dispatch :step/kind}
   [:step/clean
    [:map {:closed true}
     [:step/kind [:= :step/clean]]
     [:step/path [:string {:min 1}]]]]

   [:step/compile
    [:map {:closed true}
     [:step/kind [:= :step/compile]]
     [:step/src-dirs [:vector [:string {:min 1}]]]
     [:step/ns-compile [:vector NsSymbol]]
     [:step/class-dir [:string {:min 1}]]
     [:step/elide-meta [:vector :keyword]]
     [:step/java-opts [:vector :string]]]]

   [:step/copy-classes
    [:map {:closed true}
     [:step/kind [:= :step/copy-classes]]
     [:step/from [:string {:min 1}]]
     [:step/to [:string {:min 1}]]
     [:step/prefixes [:vector [:string {:min 1}]]]
     [:step/files [:vector [:string {:min 1}]]]]]

   [:step/copy-dir
    [:map {:closed true}
     [:step/kind [:= :step/copy-dir]]
     [:step/src-dirs [:vector [:string {:min 1}]]]
     [:step/target-dir [:string {:min 1}]]]]

   [:step/write-pom
    [:map {:closed true}
     [:step/kind [:= :step/write-pom]]]]

   [:step/jar
    [:map {:closed true}
     [:step/kind [:= :step/jar]]
     [:step/class-dir [:string {:min 1}]]
     [:step/jar-file [:string {:min 1}]]]]

   [:step/normalize
    [:map {:closed true}
     [:step/kind [:= :step/normalize]]
     [:step/path [:string {:min 1}]]]]

   [:step/verify-load
    [:map {:closed true}
     [:step/kind [:= :step/verify-load]]
     [:step/jar-file [:string {:min 1}]]
     [:step/namespaces [:vector NsSymbol]]
     [:step/java-opts [:vector :string]]]]

   [:step/publish
    [:map {:closed true}
     [:step/kind [:= :step/publish]]
     [:step/target-id :keyword]
     [:step/installer [:enum :local :remote]]]]

   [:step/announce
    [:map {:closed true}
     [:step/kind [:= :step/announce]]
     [:step/message [:string {:min 1}]]]]])

(def Plan
  [:vector Step])
