(ns hive-build.collect.git
  "Reads from the repository, through git.

   Every function here is one effect and no decision. Nothing interprets what
   it reads, and nothing throws: a git that is absent, or a repository that has
   never released, answers the same way an empty result does."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def ^:private record-separator
  "NUL between commit records, emitted by git's own %x00.

   A commit body contains blank lines and may contain any printable text, so
   no printable delimiter can separate two of them safely."
  (str (char 0)))

(defn run
  "Run git with `args`; {:ok? bool :out string :err string}. Never throws."
  [& args]
  (try
    (let [{:keys [exit out err]} (apply shell/sh "git" args)]
      {:ok? (zero? exit) :out (str/trim (or out "")) :err (str/trim (or err ""))})
    (catch Throwable t
      {:ok? false :out "" :err (.getMessage t)})))

(defn release-tags
  "Every v-prefixed tag, newest version first. [] when there are none."
  []
  (let [{:keys [ok? out]} (run "tag" "--list" "v*" "--sort=-v:refname")]
    (if (and ok? (seq out))
      (into [] (remove str/blank?) (str/split-lines out))
      [])))

(defn tag-date
  "The date `tag` points at, as yyyy-MM-dd, or nil."
  [tag]
  (when tag
    (let [{:keys [ok? out]} (run "log" "-1" "--format=%ad" "--date=short" tag)]
      (when (and ok? (seq out)) out))))

(defn messages-between
  "Full commit messages reachable from `to` and not from `from`, newest first.

   `from` nil or blank reads from the root of history. [] when the range is
   empty or either ref is unknown."
  [from to]
  (let [range' (if (str/blank? (str from)) (str to) (str from ".." to))
        {:keys [ok? out]} (run "log" "--format=%B%x00" range')]
    (if (and ok? (seq out))
      (into [] (comp (map str/trim) (remove str/blank?))
            (str/split out (re-pattern (java.util.regex.Pattern/quote record-separator))))
      [])))
