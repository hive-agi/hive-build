(ns hive-build.promote.classes-test
  "The AOT foreign-class audit."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-build.promote.classes :as classes]))

(defn- class-bytes
  "The bytes of a class already on this JVM's classpath."
  [internal-name]
  (with-open [in  (io/input-stream (io/resource (str internal-name ".class")))
              out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(deftest class-names-reads-a-real-constant-pool
  (let [names (classes/class-names (class-bytes "clojure/lang/PersistentVector"))]
    (testing "the class knows itself and its supertypes"
      (is (contains? names "clojure/lang/PersistentVector"))
      (is (contains? names "clojure/lang/APersistentVector")))
    (testing "array descriptors are unwrapped to the element class"
      (is (not-any? #(re-find #"^\[" %) names)))
    (testing "no descriptor punctuation survives"
      (is (not-any? #(re-find #";" %) names)))))

(deftest internal-name-accepts-either-spelling
  (is (= "hive_addon/protocol/IAddon" (classes/internal-name "hive-addon.protocol.IAddon")))
  (is (= "hive_addon/protocol/IAddon" (classes/internal-name "hive_addon/protocol/IAddon")))
  (is (= "hive_addon/protocol/IAddon" (classes/internal-name 'hive-addon.protocol.IAddon))))

(deftest foreign-refs-flags-only-what-the-jar-cannot-satisfy
  (let [names ["hive_thing/core$fn" "hive_addon/protocol/IAddon"
               "hive_spi/memory/ports/IMemoryStore" "clojure/lang/RT"
               "java/lang/Object"]]
    (testing "own classes, shipped classes and declared classes all pass"
      (is (= ["hive_spi/memory/ports/IMemoryStore"]
             (classes/foreign-refs names {:prefixes ["hive_thing/"]
                                          :shipped #{"hive_addon/protocol/IAddon"}
                                          :allowed #{}}))))
    (testing "an explicit allowance silences a link the jar does not ship"
      (is (= [] (classes/foreign-refs names
                                      {:prefixes ["hive_thing/"]
                                       :shipped #{"hive_addon/protocol/IAddon"}
                                       :allowed #{"hive_spi/memory/ports/IMemoryStore"}}))))
    (testing "non-hive classes are never audited"
      (is (= [] (classes/foreign-refs ["clojure/lang/RT" "java/lang/Object"]
                                      {:prefixes [] :shipped #{} :allowed #{}}))))
    (testing "the audited prefix set is caller-supplied"
      (is (= ["other_lib/Thing"]
             (classes/foreign-refs ["other_lib/Thing" "hive_x/Y"]
                                   {:prefixes [] :shipped #{} :allowed #{}
                                    :audited ["other_lib"]}))))))

(deftest report-names-every-offender-and-the-two-ways-out
  (let [message (classes/report ["hive_spi/memory/ports/IMemoryStore"])]
    (is (re-find #"hive_spi.memory.ports.IMemoryStore" message))
    (is (re-find #":aot/package-protocols" message))
    (is (re-find #":aot/allow-foreign-classes" message)))
  (testing "a clean jar reports nothing"
    (is (nil? (classes/report [])))))
