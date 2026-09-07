(ns hive-build.boundary.exclude-steps-test
  "The :step/exclude and :step/verify-excluded handlers against a real
   directory and a real zip."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-build.boundary.archive :as archive]
            [hive-build.boundary.tools :as tools]
            [hive-build.collect.io :as io']))

(defn- temp-dir []
  (let [d (java.nio.file.Files/createTempDirectory
           "hive-build-exclude" (make-array java.nio.file.attribute.FileAttribute 0))]
    (str d)))

(defn- write! [root rel content]
  (let [f (io/file root rel)]
    (io/make-parents f)
    (spit f content)))

(deftest the-exclude-step-removes-files-and-directories-beneath-class-dir
  (let [class-dir (temp-dir)]
    (write! class-dir "hive_qdrant/store.clj" "(ns hive-qdrant.store)")
    (write! class-dir "hive_qdrant/addon.clj" "(ns hive-qdrant.addon)")
    (write! class-dir "META-INF/hive-addons/hive-qdrant.edn" "{}")
    (write! class-dir "META-INF/MANIFEST.MF" "Manifest-Version: 1.0")
    (let [handler (get tools/handlers :step/exclude)
          removed (handler {} {:step/kind :step/exclude
                               :step/class-dir class-dir
                               :step/paths ["hive_qdrant/addon.clj"
                                            "META-INF/hive-addons"
                                            "never/there.clj"]})]
      (testing "only paths that existed are reported removed"
        (is (= ["hive_qdrant/addon.clj" "META-INF/hive-addons"] removed)))
      (is (not (io'/exists? (str class-dir "/hive_qdrant/addon.clj"))))
      (is (not (io'/exists? (str class-dir "/META-INF/hive-addons"))))
      (testing "everything else survives"
        (is (io'/exists? (str class-dir "/hive_qdrant/store.clj")))
        (is (io'/exists? (str class-dir "/META-INF/MANIFEST.MF")))))))

(defn- zip-with [entries]
  (let [path (str (temp-dir) "/artifact.jar")]
    (archive/write-zip! path (into {} (map (fn [e] [e (.getBytes "x" "UTF-8")])) entries))
    path))

(deftest the-verify-step-refuses-a-jar-that-still-carries-an-excluded-entry
  (let [handler (get tools/handlers :step/verify-excluded)
        paths ["hive_qdrant/addon.clj" "META-INF/hive-addons"]]
    (testing "a clean jar passes and reports no violations"
      (is (= [] (handler {} {:step/kind :step/verify-excluded
                             :step/jar-file (zip-with ["hive_qdrant/store.clj"
                                                       "META-INF/MANIFEST.MF"])
                             :step/paths paths}))))
    (testing "an excluded file or a manifest beneath an excluded directory fails"
      (let [jar (zip-with ["hive_qdrant/store.clj" "hive_qdrant/addon.clj"
                           "META-INF/hive-addons/" "META-INF/hive-addons/hive-qdrant.edn"])
            thrown (try (handler {} {:step/kind :step/verify-excluded
                                     :step/jar-file jar
                                     :step/paths paths})
                        nil
                        (catch clojure.lang.ExceptionInfo e e))]
        (is (some? thrown))
        (is (= ["META-INF/hive-addons/" "META-INF/hive-addons/hive-qdrant.edn"
                "hive_qdrant/addon.clj"]
               (:violations (ex-data thrown))))
        (is (re-find #"3 entries" (ex-message thrown)))))))
