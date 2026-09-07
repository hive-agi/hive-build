(ns hive-build.collect.proc
  "Runs a child process. One effect, no decision, never throws.

   The exit code is the answer; stdout and stderr ride along untrimmed so a
   caller can show a program's own report verbatim."
  (:require [clojure.java.shell :as shell]))

(defn run
  "Run `command` with `args`; {:ok? bool :exit int :out string :err string}.
   `opts` may carry :dir, the working directory. Never throws."
  [opts command & args]
  (try
    (let [{:keys [exit out err]} (apply shell/sh command
                                        (concat args (when-let [d (:dir opts)] [:dir d])))]
      {:ok? (zero? exit) :exit exit :out (str (or out "")) :err (str (or err ""))})
    (catch Throwable t
      {:ok? false :exit -1 :out "" :err (str (.getMessage t))})))
