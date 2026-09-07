(ns hive-build.api-readme-examples-test
  "The readme-examples task entry point, with its effects substituted."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-build.api :as api]
            [hive-build.collect.io :as io']
            [hive-build.collect.proc :as proc]))

(def ^:private readme "# x\n\n```clojure\n(+ 1 2)\n;; => 3\n```\n")

(deftest a-readme-with-no-runnable-block-claims-nothing-and-runs-nothing
  (let [ran (atom 0)]
    (with-redefs [io'/read-text (constantly "# x\n\n```sh\nls\n```\n")
                  proc/run (fn [& _] (swap! ran inc) {:ok? true :exit 0 :out "" :err ""})]
      (is (= {:examples/blocks 1 :examples/runnable 0 :examples/skipped 1 :examples/claims 0}
             (api/readme-examples {})))
      (is (zero? @ran) "nothing was claimed, so no JVM was started"))))

(deftest an-absent-readme-is-an-empty-one
  (with-redefs [io'/read-text (constantly nil)
                proc/run (fn [& _] (throw (ex-info "must not run" {})))]
    (is (= 0 (:examples/blocks (api/readme-examples {}))))))

(deftest the-program-runs-in-a-new-jvm-on-the-named-aliases
  (let [written (atom nil)
        call    (atom nil)]
    (with-redefs [io'/read-text   (constantly readme)
                  io'/write-text! (fn [path text] (reset! written [path text]) path)
                  proc/run        (fn [_ & args]
                                    (reset! call (vec args))
                                    {:ok? true :exit 0
                                     :out "readme-examples: 0 refuted claim(s)\n" :err ""})]
      (let [r (api/readme-examples {:aliases [:test :x] :out "target/t.clj"})]
        (is (= ["clojure" "-M:test:x" "target/t.clj"] @call))
        (is (= "target/t.clj" (first @written)))
        (is (str/includes? (second @written) "(+ 1 2)"))
        (is (= {:examples/blocks 1 :examples/runnable 1 :examples/skipped 0
                :examples/claims 1 :examples/ok? true}
               r))))))

(deftest no-aliases-means-the-project-classpath
  (let [call (atom nil)]
    (with-redefs [io'/read-text   (constantly readme)
                  io'/write-text! (fn [path _] path)
                  proc/run        (fn [_ & args] (reset! call (vec args))
                                    {:ok? true :exit 0 :out "" :err ""})]
      (api/readme-examples {})
      (is (= ["clojure" "-M" "target/readme-examples.clj"] @call)))))

(deftest a-red-exit-refuses-and-carries-the-counts
  (with-redefs [io'/read-text   (constantly readme)
                io'/write-text! (fn [path _] path)
                proc/run        (fn [& _] {:ok? false :exit 1
                                           :out "readme-examples: block 0 line 1 claims 3 but got 4\n"
                                           :err ""})]
    (let [e (is (thrown? clojure.lang.ExceptionInfo (api/readme-examples {})))]
      (is (= 1 (:examples/exit (ex-data e))))
      (is (= 1 (:examples/claims (ex-data e)))))))
