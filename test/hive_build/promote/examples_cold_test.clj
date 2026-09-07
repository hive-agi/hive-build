(ns hive-build.promote.examples-cold-test
  "The generated program, run the way the task runs it: in a new JVM.

   These are the only tests here that spawn a process. They are the rung the
   pure suite cannot reach: that a claim which holds exits zero, that a
   refuted one exits non-zero and says which, and that a form that throws
   fails the run after naming itself."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-build.collect.io :as io']
            [hive-build.collect.proc :as proc]
            [hive-build.promote.examples :as ex])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private clojure-on-path?
  (:ok? (proc/run {} "clojure" "--version")))

(defn- run-readme [markdown]
  (let [dir  (str (Files/createTempDirectory "hive-build-readme" (make-array FileAttribute 0)))
        path (str dir "/readme-examples.clj")]
    (io'/write-text! path (ex/script (ex/blocks markdown)))
    (proc/run {} "clojure" "-M" path)))

(deftest a-readme-whose-claims-hold-exits-zero
  (if-not clojure-on-path?
    (println "examples-cold-test: clojure is not on PATH, the cold run was not measured")
    (let [{:keys [ok? out err]}
          (run-readme "```clojure\n(+ 1 2)\n;; => 3\n(str \"a\" \"b\")\n;; => \"ab\"\n(def x 1)\n```")]
      (is ok? (str out err))
      (is (str/includes? out "readme-examples: 0 refuted claim(s)")))))

(deftest a-refuted-claim-exits-non-zero-and-says-which
  (when clojure-on-path?
    (let [{:keys [ok? out]} (run-readme "```clojure\n(+ 1 2)\n;; => 4\n```")]
      (is (not ok?))
      (is (str/includes? out "block 0 line 1 claims 4 but got 3")))))

(deftest a-form-that-throws-fails-the-run-after-naming-itself
  (when clojure-on-path?
    (let [{:keys [ok? out err]}
          (run-readme "```clojure\n(+ 1 2)\n;; => 3\n(throw (ex-info \"boom\" {}))\n```")]
      (is (not ok?))
      (is (str/includes? out "readme-examples: block 0 line 3"))
      (is (str/includes? (str out err) "boom")))))
