(ns hive-build.promote.naming-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-schemas.test :as st]
            [hive-build.promote.naming :as naming]
            [hive-build.schema :as s]))

(def TargetDir [:enum "target" "build/out" "tmp"])
(def RepoUrl [:enum "https://repo.clojars.org"
              "https://gitea.hive-mcp.com/api/packages/hive-agi/maven"
              "https://example.test/maven/"])

;; ── Synthesized from the schemas ───────────────────────────────────────────

(st/deftrifecta-from-schema coordinate
  hive-build.promote.naming/coordinate
  {:in [:cat s/LibSymbol s/VersionString]
   :out s/Coordinate
   :rel (fn [[lib v] out]
          (and (= lib (naming/lib-symbol out))
               (= v (:coordinate/version out))))
   :num-tests 200})

(st/deftrifecta-from-schema lib-symbol
  hive-build.promote.naming/lib-symbol
  {:in s/Coordinate
   :out s/LibSymbol
   :rel (fn [in out] (= in (naming/coordinate out (:coordinate/version in))))
   :mutation false
   :num-tests 200})

(st/deftrifecta-from-schema jar-file
  hive-build.promote.naming/jar-file
  {:in [:cat TargetDir s/Coordinate]
   :out [:string {:min 1}]
   :rel (fn [[dir coord] out]
          (and (str/starts-with? out (str dir "/"))
               (str/ends-with? out ".jar")
               (str/includes? out (:coordinate/artifact-id coord))
               (str/includes? out (:coordinate/version coord))))
   :mutation false
   :num-tests 200})

(st/deftrifecta-from-schema pom-url
  hive-build.promote.naming/pom-url
  {:in [:cat RepoUrl s/Coordinate]
   :out [:string {:min 1}]
   :rel (fn [[url coord] out]
          (let [{:coordinate/keys [group-id artifact-id version]} coord]
            (and (str/starts-with? out (str/replace url #"/+$" ""))
                 (str/ends-with? out (str artifact-id "-" version ".pom"))
                 (str/includes? out (str "/" (str/replace group-id "." "/") "/"))
                 (not (str/includes? out "//maven")))))
   :mutation false
   :num-tests 200})

(st/deftrifecta-from-schema ns->path
  hive-build.promote.naming/ns->path
  {:in s/NsSymbol
   :out [:string {:min 1}]
   ;; The correspondence AOT compilation depends on, both ways.
   :rel (fn [in out]
          (and (= in (naming/path->ns out))
               (not (str/includes? out "-"))
               (not (str/includes? out "."))))
   :mutation false
   :num-tests 300})

;; ── What a schema cannot state ────────────────────────────────────────────

(deftest ns-path-correspondence-is-the-clojure-file-convention
  (is (= "hive_build/promote/naming" (naming/ns->path 'hive-build.promote.naming)))
  (is (= 'hive-build.promote.naming (naming/path->ns "hive_build/promote/naming"))))

(deftest coordinate-of-an-unqualified-lib-uses-the-name-for-both
  (testing "a lib symbol with no group is its own group, as tools.build reads it"
    (is (= {:coordinate/group-id "thing"
            :coordinate/artifact-id "thing"
            :coordinate/version "1.0.0"}
           (naming/coordinate 'thing "1.0.0")))))

(deftest pom-url-is-the-immutability-probe
  (testing "the exact document whose presence means this coordinate is spent"
    (is (= (str "https://repo.clojars.org/io/github/hive-agi/hive-build"
                "/0.1.0/hive-build-0.1.0.pom")
           (naming/pom-url "https://repo.clojars.org"
                           {:coordinate/group-id "io.github.hive-agi"
                            :coordinate/artifact-id "hive-build"
                            :coordinate/version "0.1.0"}))))
  (testing "a trailing slash on the registry does not produce a doubled slash"
    (is (= (naming/pom-url "https://x.test/maven" {:coordinate/group-id "g"
                                                   :coordinate/artifact-id "a"
                                                   :coordinate/version "1.0.0"})
           (naming/pom-url "https://x.test/maven/" {:coordinate/group-id "g"
                                                    :coordinate/artifact-id "a"
                                                    :coordinate/version "1.0.0"})))))

(deftest own-class-admits-only-this-projects-namespaces
  (let [prefixes ["hive_build/promote" "hive_build/schema"]]
    (testing "own classes pass"
      (is (naming/own-class? prefixes "hive_build/promote/naming__init.class"))
      (is (naming/own-class? prefixes "hive_build/schema$fn__1.class")))
    (testing "a preloaded host namespace compiled in the same JVM does not"
      (is (not (naming/own-class? prefixes "hive_addon/protocol__init.class")))
      (is (not (naming/own-class? prefixes "clojure/core__init.class"))))
    (testing "a non-class file under an own prefix does not"
      (is (not (naming/own-class? prefixes "hive_build/promote/naming.clj"))))
    (testing "no prefixes admits nothing"
      (is (not (naming/own-class? [] "hive_build/schema__init.class"))))))

(deftest a-protocol-symbol-names-its-generated-interface
  (is (= "hive_addon/protocol/IAddon.class"
         (naming/protocol->class-path 'hive-addon.protocol/IAddon))))

(deftest own-class-is-prefix-not-substring
  (testing "a host namespace that merely contains an own prefix is excluded"
    (is (not (naming/own-class? ["hive_build"] "vendor/hive_build/x__init.class")))))
