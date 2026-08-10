(ns hive-build.boundary.load-verify-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.tools.build.api :as b]
            [hive-build.boundary.archive :as archive]
            [hive-build.boundary.load-verify :as load-verify])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (str (Files/createTempDirectory "hive-build-load-test"
                                  (make-array FileAttribute 0))))

(defn- write-source! [src-dir relative-path content]
  (let [file (io/file src-dir relative-path)]
    (io/make-parents file)
    (spit file content)))

(defn- compile-fixture! [root]
  (let [src-dir (str root "/src")
        scratch-dir (str root "/scratch")
        class-dir (str root "/classes")
        jar-file (str root "/fixture.jar")
        compile-basis (b/create-basis {:project "deps.edn"
                                       :extra {:paths [src-dir]}})]
    (write-source! src-dir "host/protocol.clj"
                   "(ns host.protocol)\n(defprotocol IHost (ping [this]))\n")
    (write-source! src-dir "fixture/impl.clj"
                   (str "(ns fixture.impl (:require [host.protocol]))\n"
                        "(defrecord Impl [] host.protocol/IHost (ping [_] :pong))\n"))
    (b/compile-clj {:basis compile-basis
                    :src-dirs [src-dir]
                    :ns-compile ['host.protocol 'fixture.impl]
                    :class-dir scratch-dir})
    (archive/copy-own-classes! scratch-dir class-dir ["fixture/impl"])
    (b/jar {:class-dir class-dir :jar-file jar-file})
    {:jar-file jar-file
     :scratch-dir scratch-dir
     :class-dir class-dir
     :namespaces ['fixture.impl]
     :declared-basis (b/create-basis {:project "deps.edn"})
     :compile-basis compile-basis}))

(deftest missing-foreign-interface-fails-before-publication
  (let [{:keys [jar-file namespaces declared-basis]} (compile-fixture! (temp-dir))]
    (testing "the exact copy-own-classes failure: interface compiled, then dropped"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"not loadable on its declared dependency classpath"
           (load-verify/verify! {:basis declared-basis
                                 :jar-file jar-file
                                 :namespaces namespaces
                                 :java-opts []}))))))

(deftest an-explicitly-packaged-interface-loads
  (let [{:keys [jar-file scratch-dir class-dir namespaces compile-basis]}
        (compile-fixture! (temp-dir))]
    (archive/copy-own-classes! scratch-dir class-dir [] ["host/protocol/IHost.class"])
    (b/jar {:class-dir class-dir :jar-file jar-file})
    (is (= jar-file
           (load-verify/verify! {:basis compile-basis
                                 :jar-file jar-file
                                 :namespaces namespaces
                                 :java-opts []})))))
