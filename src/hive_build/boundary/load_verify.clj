(ns hive-build.boundary.load-verify
  "Verify the built AOT jar in a fresh JVM containing only its declared
   dependency classpath. Provided/overlay dependencies are deliberately absent."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]))

(defn declared-classpath
  [basis jar-file]
  (str/join java.io.File/pathSeparator
            (cons (.getAbsolutePath (io/file jar-file))
                  (:classpath-roots basis))))

(defn require-expression
  [namespaces]
  (pr-str `(doseq [namespace# '~namespaces]
             (require namespace#))))

(defn command
  [{:keys [basis jar-file namespaces java-opts]}]
  (into [(str (System/getProperty "java.home")
              java.io.File/separator "bin" java.io.File/separator "java")]
        (concat java-opts
                ["-cp" (declared-classpath basis jar-file)
                 "clojure.main" "-e" (require-expression namespaces)])))

(defn verify!
  "Require every packaged namespace in a fresh JVM. The classpath is the jar
   plus its committed/POM dependency basis, never local.deps.edn :provided.
   Throws before publication when any foreign class is missing."
  [request]
  (let [{:keys [exit out err]} (b/process {:command-args (command request)
                                           :out :capture
                                           :err :capture})]
    (when (pos? exit)
      (throw (ex-info "AOT jar is not loadable on its declared dependency classpath"
                      {:jar-file (:jar-file request)
                       :namespaces (:namespaces request)
                       :exit exit
                       :out out
                       :err err})))
    (:jar-file request)))
