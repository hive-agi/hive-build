(ns hive-build.collect.io
  "Filesystem, environment and registry reads.

   Every function here is one effect and no decision, so everything above this
   layer can be pure. Nothing in here interprets what it reads."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn exists?
  [path]
  (.exists (io/file path)))

(defn read-text
  "Contents of `path`, or nil when it is absent."
  [path]
  (let [f (io/file path)]
    (when (.isFile f) (slurp f))))

(defn read-edn
  "Parsed EDN at `path`, or nil when the file is absent.

   A parse failure is rethrown naming the file: the underlying message says
   only what was wrong, never where, and a build tool reads several EDN files."
  [path]
  (when-let [text (read-text path)]
    (try
      (edn/read-string text)
      (catch Exception e
        (throw (ex-info (str "could not parse " path ": " (ex-message e))
                        {:path path} e))))))

(defn write-text!
  [path text]
  (spit (io/file path) text)
  path)

(defn files-under
  "Paths of every regular file beneath `dir`, or () when `dir` is absent."
  [dir]
  (into [] (comp (filter #(.isFile ^java.io.File %))
                 (map #(.getPath ^java.io.File %)))
        (file-seq (io/file dir))))

(defn files-by-dir
  "{dir -> paths beneath it} for `dirs`."
  [dirs]
  (into {} (map (juxt identity files-under)) dirs))

(defn declared-ns
  "The namespace name declared in the source file at `path`, or nil.

   Reads forms until it finds the ns form, so a leading comment or reader
   conditional does not hide it."
  [path]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (loop []
      (let [form (try (read {:read-cond :allow :eof ::eof} r)
                      (catch Exception _ ::eof))]
        (cond
          (= form ::eof) nil
          (and (seq? form) (= 'ns (first form))) (second form)
          :else (recur))))))

(def ^:private spdx-pattern
  ;; Identifier charset only — \S+ would swallow a trailing quote from an SPDX
  ;; line inside a string literal and report a phantom conflict against the
  ;; identical bare identifier.
  #"SPDX-License-Identifier:\s*([A-Za-z0-9.+-]+)")

(defn spdx-ids
  "Distinct SPDX-License-Identifier values declared across `paths`."
  [paths]
  (into #{} (keep #(second (re-find spdx-pattern (or (read-text %) "")))) paths))

(defn getenv
  [k]
  (System/getenv k))

(defn env
  "{k -> value} for `ks`.

   Throws when any is unset or blank: a deploy that proceeds with a missing
   credential fails at the registry, after the artifact has been built."
  [ks]
  (reduce (fn [acc k]
            (let [v (getenv k)]
              (if (str/blank? v)
                (throw (ex-info (str k " not set or blank") {:env k}))
                (assoc acc k v))))
          {}
          ks))

(defn head-ok?
  "True when a HEAD of `url` answers 200. Any failure is false: an unreachable
   registry must not be read as `already published`."
  [url auth]
  (try
    (let [conn (doto ^java.net.HttpURLConnection
                     (.openConnection (java.net.URI. url))
                 (.setRequestMethod "HEAD")
                 (.setConnectTimeout 10000)
                 (.setReadTimeout 10000))]
      (when auth (.setRequestProperty conn "Authorization" auth))
      (= 200 (.getResponseCode conn)))
    (catch Throwable _ false)))

(defn on-path?
  "True when `command` answers successfully to `probe-args`."
  [command probe-args run-process]
  (try (zero? (:exit (run-process (into [command] probe-args))))
       (catch Throwable _ false)))
