(ns hive-build.promote.opacity-test
  "The artifact audit: does the built jar still carry the words its sources
   declared private?

   The last group compiles a namespace for real and reads the constant pool of
   the class that comes out, because the question this namespace exists to
   answer is about an artifact and cannot be settled by inspecting config."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-build.promote.classes :as classes]
            [hive-build.promote.elide :as elide]
            [hive-build.promote.opacity :as opacity])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; ── Secrets, read out of source ───────────────────────────────────────────

(deftest source-secrets-collects-both-elide-proof-classes
  (let [src (str "(ns acme.core\n  \"The reason this namespace exists, at length.\")\n"
                 "(defprotocol IRank\n"
                 "  (rank [this xs] \"The weighting the customer is paying for.\"))\n"
                 "(defn f \"An ordinary def docstring, long enough to count.\" [x] x)\n")
        secrets (opacity/source-secrets src)]
    (is (contains? secrets "The reason this namespace exists, at length."))
    (is (contains? secrets "The weighting the customer is paying for."))
    (testing "def docstrings are not audited: :elide-meta already removes them,
              and auditing them would make this a second implementation of the
              compiler rather than a check on the artifact"
      (is (not (contains? secrets "An ordinary def docstring, long enough to count."))))))

(deftest short-docstrings-are-not-audited
  (testing "a one-word docstring collides with member names and descriptors"
    (let [src "(defprotocol I (m [this] \"Ranks.\"))\n"]
      (is (empty? (opacity/source-secrets src))))))

(deftest the-audited-string-excludes-its-own-quotes
  (let [src "(ns acme.core \"Twenty four characters ok.\")\n"]
    (is (= #{"Twenty four characters ok."} (opacity/source-secrets src))
        "the artifact carries the string's characters, not its literal syntax")))

;; ── The audit itself ──────────────────────────────────────────────────────

(deftest a-secret-present-in-a-class-is-reported-against-that-class
  (let [result (opacity/audit
                {:secrets #{"The weighting the customer pays for."}
                 :constants-by-entry {"acme/kernel__init.class"
                                      #{"rank" "The weighting the customer pays for."}
                                      "acme/other__init.class" #{"rank"}}
                 :entries ["acme/kernel__init.class" "acme/other__init.class"]})]
    (is (= :opacity/leaking (:opacity/verdict result)))
    (is (= 1 (count (:opacity/findings result))))
    (is (= "acme/kernel__init.class" (:finding/where (first (:opacity/findings result)))))))

(deftest an-artifact-carrying-none-of-them-is-clean
  (let [result (opacity/audit
                {:secrets #{"The weighting the customer pays for."}
                 :constants-by-entry {"acme/kernel__init.class" #{"rank" "invoke"}}
                 :entries ["acme/kernel__init.class"]})]
    (is (= :opacity/clean (:opacity/verdict result)))
    (is (empty? (:opacity/findings result)))
    (is (nil? (opacity/report result)))))

(deftest one-secret-in-two-classes-is-two-findings
  (testing "a finding names where the operator has to go"
    (let [result (opacity/audit
                  {:secrets #{"Shared secret text."}
                   :constants-by-entry {"a__init.class" #{"Shared secret text."}
                                        "b__init.class" #{"Shared secret text."}}
                   :entries []})]
      (is (= 2 (count (:opacity/findings result))))
      (is (= #{"a__init.class" "b__init.class"}
             (set (map :finding/where (:opacity/findings result))))))))

(deftest clojure-sources-in-the-artifact-are-a-finding-of-their-own
  (let [result (opacity/audit
                {:secrets #{}
                 :constants-by-entry {}
                 :entries ["acme/kernel.clj" "acme/kernel__init.class" "acme/x.cljc"]})]
    (is (= :opacity/leaking (:opacity/verdict result)))
    (is (= #{"acme/kernel.clj" "acme/x.cljc"}
           (set (map :finding/where (:opacity/findings result)))))
    (is (every? #(= :finding/source-entry (:finding/kind %)) (:opacity/findings result)))))

(deftest sources-published-on-purpose-are-not-findings
  (testing "clj-kondo cannot read a compiled hook, so hook source must ship"
    (let [result (opacity/audit
                  {:secrets #{}
                   :constants-by-entry {}
                   :entries ["clj-kondo.exports/acme/hooks/m.clj" "acme/kernel.clj"]
                   :allowed-source ["clj-kondo.exports"]})]
      (is (= ["acme/kernel.clj"] (map :finding/where (:opacity/findings result)))))))

(deftest the-report-names-the-entry-and-does-not-reprint-the-secret-whole
  (let [secret (apply str "S" (repeat 200 "x"))
        result (opacity/audit {:secrets #{secret}
                               :constants-by-entry {"acme/k__init.class" #{secret}}
                               :entries []})
        message (opacity/report result)]
    (is (str/includes? message "acme/k__init.class"))
    (is (not (str/includes? message secret))
        "a build log is less protected than the artifact it describes")
    (is (str/includes? message "..."))))

(deftest a-clean-audit-still-says-how-much-it-checked
  (testing "a verdict from an empty secret set is not evidence of opacity"
    (let [result (opacity/audit {:secrets #{} :constants-by-entry {} :entries []})]
      (is (= :opacity/clean (:opacity/verdict result)))
      (is (= 0 (:opacity/secrets-audited result))))))

;; ── Artifact rung: compile it and read the class ──────────────────────────

(defn- tmpdir [prefix]
  (str (Files/createTempDirectory prefix (into-array FileAttribute []))))

(defn- ns-path [ns-sym]
  (-> (str ns-sym) (str/replace "-" "_") (str/replace "." "/")))

(defn- compile-probe!
  "Compile `source` as `ns-sym` under the fleet's :elide-meta, then read the
   constant pool of the __init class the compiler produced.

   Returns `{:constants set :loader cl}`. The loader is handed back so a caller
   that wants to LOAD what it just compiled can bind it too."
  [source ns-sym]
  (let [src-dir (tmpdir "opacity-src")
        out-dir (tmpdir "opacity-out")
        file (io/file src-dir (str (ns-path ns-sym) ".clj"))
        ;; The test runner's base loader is the app loader, which cannot take a
        ;; new URL. The compiler reads its loader from Compiler/LOADER when that
        ;; var is bound, so the probe supplies its own for the duration.
        loader (doto (clojure.lang.DynamicClassLoader. (clojure.lang.RT/baseLoader))
                 (.addURL (.toURL (.toURI (io/file src-dir))))
                 (.addURL (.toURL (.toURI (io/file out-dir)))))]
    (io/make-parents file)
    (spit file source)
    (with-bindings {clojure.lang.Compiler/LOADER loader
                    #'*compile-path* out-dir
                    #'*compiler-options* {:elide-meta [:doc :file :line :added :arglists]}}
      (compile ns-sym))
    (let [init (io/file out-dir (str (ns-path ns-sym) "__init.class"))]
      (is (.exists init) "the compile produced no __init class")
      {:constants (classes/utf8-constants (Files/readAllBytes (.toPath init)))
       :loader loader})))

(defn- compiled-constants
  [source ns-sym]
  (:constants (compile-probe! source ns-sym)))

(defn- probe-source [ns-sym]
  (str "(ns " ns-sym "\n  \"NS-SECRET the reason this namespace exists.\")\n"
       "(defprotocol IRank\n"
       "  (rank [this xs] \"PROTO-SECRET the weighting they pay for.\"))\n"
       "(defn f \"DEF-SECRET an ordinary docstring.\" [x] x)\n"))

(defn- carries? [constants marker]
  (boolean (some #(str/includes? % marker) constants)))

(deftest elide-meta-alone-leaves-two-of-the-three-docstrings-in-the-class
  (let [constants (compiled-constants (probe-source 'hivebuildprobe.plain)
                                      'hivebuildprobe.plain)]
    (testing "the compiler option does its own job"
      (is (not (carries? constants "DEF-SECRET"))))
    (testing "and reaches neither docstring that is form DATA rather than metadata"
      (is (carries? constants "NS-SECRET")
          "the ns macro emits its docstring inside a constant map")
      (is (carries? constants "PROTO-SECRET")
          "defprotocol records :doc in :sigs, which is the protocol var's value"))))

(deftest the-staged-elision-removes-what-the-compiler-cannot
  (let [source (elide/without-docstrings (probe-source 'hivebuildprobe.staged))
        constants (compiled-constants source 'hivebuildprobe.staged)]
    (is (not (carries? constants "NS-SECRET")))
    (is (not (carries? constants "PROTO-SECRET")))
    (is (not (carries? constants "DEF-SECRET")))
    (testing "the method name survives, because dispatch is by name"
      (is (contains? constants "rank")))))

(deftest an-elided-protocol-still-dispatches
  (testing "opacity that breaks the product is not a trade anyone would take"
    (let [ns-sym 'hivebuildprobe.live
          source (elide/without-docstrings (probe-source ns-sym))
          {:keys [loader]} (compile-probe! source ns-sym)]
      (with-bindings {clojure.lang.Compiler/LOADER loader}
        (require ns-sym)
        (eval '(defrecord ProbeRank []
                 hivebuildprobe.live/IRank
                 (rank [_ xs] (count xs))))
        (is (= 3 (eval '(hivebuildprobe.live/rank (->ProbeRank) [:a :b :c]))))))))

(deftest the-audit-catches-the-unelided-class-end-to-end
  (testing "source secrets against a real constant pool, with no stub between"
    (let [source (probe-source 'hivebuildprobe.endtoend)
          result (opacity/audit
                  {:secrets (opacity/source-secrets source)
                   :constants-by-entry {"hivebuildprobe/endtoend__init.class"
                                        (compiled-constants source 'hivebuildprobe.endtoend)}
                   :entries []})]
      (is (= :opacity/leaking (:opacity/verdict result)))
      (is (= 2 (count (:opacity/findings result)))
          "the ns docstring and the protocol docstring, not the def docstring"))))
