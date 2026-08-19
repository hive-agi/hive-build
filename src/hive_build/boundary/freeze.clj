(ns hive-build.boundary.freeze
  "Everything the freeze gate must read from the world: source text at HEAD and
   at the previous release tag, that tag's age, and this commit's message.

   Isolated here so `promote.freeze` and `promote.api-surface` stay pure and
   testable without a repository."
  (:require [clojure.java.io :as jio]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [hive-build.collect.io :as io'])
  (:import [java.io PushbackReader StringReader]
           [java.time Instant Duration]))

(defn- git
  "Run git with `args`; {:ok? bool :out string}. Never throws."
  [& args]
  (try
    (let [{:keys [exit out err]} (apply shell/sh "git" args)]
      {:ok? (zero? exit) :out (str/trim (or out "")) :err (str/trim (or err ""))})
    (catch Throwable t
      {:ok? false :out "" :err (.getMessage t)})))

(defn last-release-tag
  "The most recent v-prefixed tag, or nil when the repo has never released."
  []
  (let [{:keys [ok? out]} (git "tag" "--list" "v*" "--sort=-creatordate")]
    (when ok?
      (first (remove str/blank? (str/split-lines out))))))

(defn tag-age-days
  "Whole days since `tag` was created, or nil."
  [tag]
  (when tag
    (let [{:keys [ok? out]} (git "log" "-1" "--format=%cI" tag)]
      (when (and ok? (not (str/blank? out)))
        (try
          (.toDays (Duration/between (Instant/parse out) (Instant/now)))
          (catch Throwable _ nil))))))

(defn head-commit-message
  "Subject + body of HEAD, or \"\"."
  []
  (:out (git "log" "-1" "--format=%B")))

(defn files-at
  "Paths under `dirs` tracked at `ref`."
  [ref dirs]
  (let [{:keys [ok? out]} (apply git "ls-tree" "-r" "--name-only" ref "--" dirs)]
    (if ok?
      (->> (str/split-lines out)
           (remove str/blank?)
           (filter #(re-find #"\.clj[cs]?$" %))
           vec)
      [])))

(defn text-at
  "File content at `ref`, or nil."
  [ref path]
  (let [{:keys [ok? out]} (git "show" (str ref ":" path))]
    (when ok? out)))

(defn read-forms
  "Every top-level form in `text`, reader-conditionals allowed.

   A file that will not read yields [] rather than throwing: the gate must be
   able to report on a tree it cannot fully parse, and a parse failure is not
   evidence of a breaking change."
  [text]
  (when text
    (try
      (with-open [r (PushbackReader. (StringReader. text))]
        (let [eof (Object.)]
          (loop [acc []]
            (let [form (read {:eof eof :read-cond :allow} r)]
              (if (identical? eof form)
                acc
                (recur (conj acc form)))))))
      (catch Throwable _ []))))

(defn source-files
  "Clojure source paths under `dirs` on disk."
  [dirs]
  (->> dirs
       (mapcat (fn [d]
                 (when (.exists (jio/file d))
                   (->> (file-seq (jio/file d))
                        (filter #(.isFile ^java.io.File %))
                        (map #(.getPath ^java.io.File %))))))
       (filter #(re-find #"\.clj[cs]?$" %))
       vec))

(defn read-text
  "File content on disk, or nil."
  [path]
  (when (io'/exists? path) (io'/read-text path)))
