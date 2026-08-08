(ns hive-build.boundary.tools-test
  "The tools.build adapter. These are wiring assertions: they check that each
   handler hands tools.build the keys tools.build actually reads.

   tools.build destructures its options and ignores anything it does not
   recognise, so a misspelled key is not an error — it is a silently skipped
   feature that reports success. `:compiler-options` instead of `:compile-opts`
   disabled metadata elision on every AOT jar the fleet published, and nothing
   failed."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [clojure.tools.build.api :as b]
            [hive-build.boundary.tools :as tools]
            [hive-build.collect.io :as io']))

(def compile-clj-options
  "Keys clojure.tools.build.api/compile-clj documents. Anything else passed to
   it is discarded without a word."
  #{:basis :class-dir :src-dirs :ns-compile :sort :compile-opts :bindings
    :filter-nses :java-cmd :java-opts :use-cp-file :out :err :out-file :err-file})

(defn- captured-compile
  "The option map the :step/compile handler hands to b/compile-clj."
  [step]
  (let [captured (atom nil)]
    (with-redefs [b/compile-clj (fn [opts] (reset! captured opts) nil)
                  tools/aot-basis (fn [_] ::basis)]
      ((get tools/handlers :step/compile) {:ctx/overlay nil} step))
    @captured))

(def compile-step
  {:step/kind :step/compile
   :step/src-dirs ["src"]
   :step/ns-compile ['hive-thing.core]
   :step/class-dir "target/aot-classes"
   :step/elide-meta [:doc :file :line :column :added :arglists]
   :step/java-opts ["-Xmx2g"]})

(deftest metadata-elision-uses-the-key-tools-build-reads
  (testing "the whole S0 opacity rung is this one key being spelled right"
    (let [opts (captured-compile compile-step)]
      (is (= {:elide-meta [:doc :file :line :column :added :arglists]}
             (:compile-opts opts)))
      (is (not (contains? opts :compiler-options))))))

(deftest no-option-is-passed-that-tools-build-would-discard
  (testing "an unrecognised key is a silently skipped feature, not an error"
    (let [opts (captured-compile compile-step)
          unknown (set/difference (set (keys opts)) compile-clj-options)]
      (is (empty? unknown) (str "tools.build will ignore: " unknown)))))

(deftest the-compile-step-carries-its-arguments-through
  (let [opts (captured-compile compile-step)]
    (is (= ["src"] (:src-dirs opts)))
    (is (= ['hive-thing.core] (:ns-compile opts)))
    (is (= "target/aot-classes" (:class-dir opts)))
    (is (= ["-Xmx2g"] (:java-opts opts)))
    (is (= ::basis (:basis opts)))))

(deftest elision-is-omitted-rather-than-emptied-when-disabled
  (testing "an explicit [] must not become {:elide-meta []}"
    (let [opts (captured-compile (assoc compile-step :step/elide-meta []))]
      (is (not (contains? opts :compile-opts))))))

(deftest java-opts-are-omitted-when-absent
  (let [opts (captured-compile (assoc compile-step :step/java-opts []))]
    (is (not (contains? opts :java-opts)))))

;; ── Version resolution ────────────────────────────────────────────────────

(deftest the-version-file-wins-when-present
  (with-redefs [io'/read-text (constantly "1.2.3\n")]
    (is (= "1.2.3" (tools/resolve-version {})))))

(deftest a-blank-version-file-falls-back-to-the-commit-count
  (testing "repos without a VERSION file still get a monotonic coordinate"
    (with-redefs [io'/read-text (constantly "  \n")
                  b/git-count-revs (constantly "417")]
      (is (= "0.7.417" (tools/resolve-version {:minor 7})))
      (is (= "0.0.417" (tools/resolve-version {}))))))
