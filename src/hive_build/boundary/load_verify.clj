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
  "One `java ... clojure.main -e` argv requiring `namespaces` in one JVM."
  [{:keys [basis jar-file namespaces java-opts]}]
  (into [(str (System/getProperty "java.home")
              java.io.File/separator "bin" java.io.File/separator "java")]
        (concat java-opts
                ["-cp" (declared-classpath basis jar-file)
                 "clojure.main" "-e" (require-expression namespaces)])))

(defn commands
  "The argvs that verify `request`. Isolated (the default) is one JVM per
   namespace: in a shared JVM the first namespace to load a dependency makes
   it present for every namespace after it, which hides exactly the AOT
   packaging fault this gate exists to catch. `:isolated? false` trades that
   guarantee for one JVM."
  [{:keys [namespaces isolated?] :or {isolated? true} :as request}]
  (if isolated?
    (mapv #(command (assoc request :namespaces [%])) namespaces)
    [(command request)]))

(defn- run-process
  "The default process port: tools.build's process, output captured."
  [{:keys [command-args]}]
  (b/process {:command-args command-args :out :capture :err :capture}))

(defn verify!
  "Require every packaged namespace in a fresh JVM, by default one JVM per
   namespace (see `commands`). The classpath is the jar plus its committed/POM
   dependency basis, never local.deps.edn :provided. Throws before publication
   naming every namespace that cannot be required cold. `:run-fn` is the
   process port: it takes {:command-args argv} and answers {:exit :out :err}."
  [{:keys [namespaces isolated? run-fn]
    :or {isolated? true run-fn run-process} :as request}]
  (let [groups (if isolated? (mapv vector namespaces) [namespaces])
        failures (into []
                       (keep (fn [[nses argv]]
                               (let [result (run-fn {:command-args argv})]
                                 (when (pos? (:exit result))
                                   (assoc result :nses nses)))))
                       (map vector groups (commands request)))]
    (when (seq failures)
      (throw (ex-info "AOT jar is not loadable on its declared dependency classpath"
                      {:jar-file (:jar-file request)
                       :namespaces namespaces
                       :isolated? (boolean isolated?)
                       :failed (into [] (mapcat :nses) failures)
                       :exit (:exit (first failures))
                       :out (:out (first failures))
                       :err (str/join "\n" (map :err failures))})))
    (:jar-file request)))
