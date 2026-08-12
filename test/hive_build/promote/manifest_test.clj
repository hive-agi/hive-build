(ns hive-build.promote.manifest-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [hive-build.promote.manifest :as manifest]))

(deftest recognizes-addon-manifests-only
  (testing "an .edn under META-INF/hive-addons is a manifest"
    (is (manifest/manifest? "target/classes/META-INF/hive-addons/hive-ingestor.edn"))
    (is (manifest/manifest? "target\\classes\\META-INF\\hive-addons\\hive-carto.edn")))
  (testing "anything else is not"
    (is (not (manifest/manifest? "target/classes/META-INF/hive-addons")))
    (is (not (manifest/manifest? "target/classes/data.edn")))
    (is (not (manifest/manifest? "target/classes/META-INF/maven/pom.properties")))))

(deftest manifests-preserves-order-and-drops-the-rest
  (is (= ["a/META-INF/hive-addons/x.edn" "a/META-INF/hive-addons/y.edn"]
         (manifest/manifests ["a/META-INF/hive-addons/x.edn"
                              "a/core.clj"
                              "a/META-INF/hive-addons/y.edn"]))))

(deftest stamp-sets-the-version-and-keeps-every-other-key
  (let [text (pr-str {:addon/id "hive.ingestor"
                      :addon/version "0.2.4"
                      :addon/capabilities #{:tools}})
        out  (edn/read-string (manifest/stamp text "0.2.9"))]
    (is (= "0.2.9" (:addon/version out)))
    (is (= "hive.ingestor" (:addon/id out)))
    (is (= #{:tools} (:addon/capabilities out)))))

(deftest stamp-adds-the-version-when-the-manifest-omits-it
  (let [out (edn/read-string (manifest/stamp (pr-str {:addon/id "hive.carto"}) "1.4.2"))]
    (is (= "1.4.2" (:addon/version out)))))

(deftest stamp-leaves-a-non-map-resource-alone
  (testing "nil means: do not rewrite this file"
    (is (nil? (manifest/stamp "[1 2 3]" "1.0.0")))
    (is (nil? (manifest/stamp "not edn (" "1.0.0")))))
