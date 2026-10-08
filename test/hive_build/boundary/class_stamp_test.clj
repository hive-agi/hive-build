(ns hive-build.boundary.class-stamp-test
  "Class entries are stamped newer than sources, so a jar shipping both loads AOT."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.tools.build.api :as b]
            [hive-build.boundary.archive :as archive])
  (:import (java.net URLClassLoader)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util.zip ZipFile)))

(defn- temp-dir []
  (str (Files/createTempDirectory "hive-build-stamp" (make-array FileAttribute 0))))

(deftest a-class-entry-is-stamped-one-zip-tick-after-everything-else
  (is (= archive/zip-epoch (archive/entry-time "a/b.clj")))
  (is (= archive/zip-epoch (archive/entry-time "a/b.cljc")))
  (is (= archive/zip-epoch (archive/entry-time "META-INF/MANIFEST.MF")))
  (is (= archive/class-epoch (archive/entry-time "a/b__init.class")))
  (is (= 2000 (- archive/class-epoch archive/zip-epoch))))

(def entry-name
  (gen/fmap (fn [[segs ext]] (str (str/join "/" segs) ext))
            (gen/tuple (gen/not-empty (gen/vector (gen/not-empty gen/string-alphanumeric) 1 4))
                       (gen/elements [".class" ".clj" ".cljc" ".cljs" ".edn" ".MF" ""]))))

(tc/defspec every-class-is-strictly-newer-than-every-non-class 200
  (prop/for-all [a entry-name b entry-name]
    (let [class? #(str/ends-with? % ".class")]
      (if (and (class? a) (not (class? b)))
        (> (archive/entry-time a) (archive/entry-time b))
        true))))

(defn- zip-times [path]
  (with-open [zf (ZipFile. (io/file path))]
    (into {} (map (fn [e] [(.getName e) (.getTime e)])) (enumeration-seq (.entries zf)))))

(defn- build-jar-with-sources!
  "AOT-compile a one-interface ns, put its source beside the classes, jar and
   normalize. Returns [jar-file ns-sym]."
  []
  (let [root (temp-dir)
        ns-sym (symbol (str "s1.stamp" (System/nanoTime)))
        src-dir (str root "/src")
        class-dir (str root "/classes")
        jar-file (str root "/stamp.jar")
        rel (str (-> (str ns-sym) (str/replace "-" "_") (str/replace "." "/")) ".cljc")
        source (io/file src-dir rel)]
    (io/make-parents source)
    (spit source (str "(ns " ns-sym ")\n(definterface IStamp (ping []))\n"))
    (b/compile-clj {:basis (b/create-basis {:project "deps.edn" :extra {:paths [src-dir]}})
                    :src-dirs [src-dir]
                    :ns-compile [ns-sym]
                    :class-dir class-dir})
    (b/copy-dir {:src-dirs [src-dir] :target-dir class-dir})
    (b/jar {:class-dir class-dir :jar-file jar-file})
    (archive/normalize-jar! jar-file)
    [jar-file ns-sym rel]))

(deftest ^:integration a-normalized-jar-with-sources-loads-its-aot-classes
  (let [[jar-file ns-sym rel] (build-jar-with-sources!)
        times (zip-times jar-file)]
    (testing "the source rides in the jar and is older than its classes"
      (is (contains? times rel))
      (is (< (get times rel)
             (get times (str/replace rel #"\.cljc$" "__init.class")))))
    (testing "clojure loads the class, not the source"
      (let [loader (URLClassLoader. (into-array [(.toURL (io/file jar-file))])
                                    (.getContextClassLoader (Thread/currentThread)))
            thread (Thread/currentThread)
            before (.getContextClassLoader thread)]
        (try
          (.setContextClassLoader thread loader)
          (binding [*use-context-classloader* true]
            (require ns-sym))
          (let [iface (ns-resolve ns-sym 'IStamp)]
            (is (class? iface))
            (is (identical? loader (.getClassLoader ^Class iface))
                "loaded from source it would live in a DynamicClassLoader"))
          (finally
            (.setContextClassLoader thread before)
            (remove-ns ns-sym)
            (dosync (commute @#'clojure.core/*loaded-libs* disj ns-sym))))))))
