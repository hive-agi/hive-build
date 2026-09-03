(ns hive-build.promote.elide-test
  "Source-level docstring elision: the two classes of docstring the compiler's
   :elide-meta cannot reach."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-build.promote.elide :as elide]))

(defn- reads-as
  "Every top-level form in `s`, so a rewrite can be compared structurally
   rather than by text."
  [s]
  (let [r (java.io.PushbackReader. (java.io.StringReader. s))
        eof (Object.)]
    (loop [acc []]
      (let [f (read {:eof eof :read-cond :preserve} r)]
        (if (identical? f eof) acc (recur (conj acc f)))))))

;; ── ns docstrings ─────────────────────────────────────────────────────────

(deftest ns-docstring-is-found-and-removed
  (let [src "(ns acme.core\n  \"The private reason this exists.\"\n  (:require [clojure.string :as str]))\n(defn f [] 1)\n"
        out (elide/without-ns-docstring src)]
    (is (not (re-find #"private reason" out)))
    (testing "the require survives, so the ns still loads"
      (is (re-find #":require" out)))
    (testing "form count is unchanged"
      (is (= (count (reads-as src)) (count (reads-as out)))))))

(deftest a-file-without-an-ns-docstring-is-passed-through
  (let [src "(ns acme.core (:require [clojure.string :as str]))\n"]
    (is (= src (elide/without-ns-docstring src)))))

(deftest a-string-that-is-not-a-docstring-is-left-alone
  (testing "the first form is not an ns form"
    (let [src "(def banner \"Not a docstring.\")\n"]
      (is (= src (elide/without-ns-docstring src))))))

;; ── defprotocol docstrings ────────────────────────────────────────────────

(deftest protocol-method-docstring-is-found
  (let [src (str "(ns acme.core)\n"
                 "(defprotocol IRank\n"
                 "  (rank [this xs]\n"
                 "    \"The weighting nobody outside pays for.\"))\n")
        out (elide/without-protocol-docstrings src)]
    (is (not (re-find #"nobody outside pays" out))
        (str "defprotocol records :doc in its :sigs map, which is the var's "
             "VALUE. :elide-meta reaches def METADATA, so this string is "
             "compiled into the __init class and ships in every AOT jar."))
    (testing "the method name and its arglist stay: dispatch needs them"
      (is (re-find #"\(rank \[this xs\]" out)))
    (testing "the result still reads as the same forms"
      (is (= (count (reads-as src)) (count (reads-as out)))))))

(deftest the-protocols-own-docstring-is-found-too
  (let [src (str "(defprotocol IRank\n"
                 "  \"How the vendor ranks things.\"\n"
                 "  (rank [this xs]))\n")
        out (elide/without-protocol-docstrings src)]
    (is (not (re-find #"How the vendor ranks" out)))
    (is (= 1 (count (reads-as out))))))

(deftest every-signature-in-one-protocol-is-reached
  (let [src (str "(defprotocol IStore\n"
                 "  (put [this k v] \"Writes the secret weighting table.\")\n"
                 "  (get* [this k] \"Reads it back out again.\")\n"
                 "  (drop* [this k]))\n")
        out (elide/without-protocol-docstrings src)]
    (is (= 2 (count (elide/protocol-docstring-spans src))))
    (is (not (re-find #"secret weighting" out)))
    (is (not (re-find #"Reads it back" out)))
    (testing "a signature with no docstring is untouched"
      (is (re-find #"\(drop\* \[this k\]\)" out)))))

(deftest a-multi-arity-signature-keeps-every-arglist
  (let [src (str "(defprotocol IRank\n"
                 "  (rank [this xs] [this xs opts] \"Only the trailing string goes.\"))\n")
        out (elide/without-protocol-docstrings src)]
    (is (not (re-find #"trailing string" out)))
    (is (re-find #"\[this xs\] \[this xs opts\]" out))))

(deftest a-leading-string-is-not-mistaken-for-a-signature-docstring
  (testing "only a TRAILING string in a signature is a docstring"
    (let [src "(defprotocol IRank\n  (\"not-a-name\" [this]))\n"]
      (is (empty? (elide/protocol-docstring-spans src))))))

(deftest forms-that-are-not-defprotocol-are-never-touched
  (let [src (str "(defn rank\n  \"An ordinary docstring, which :elide-meta removes.\"\n  [xs] xs)\n"
                 "(comment (defprotocol Fake))\n")]
    (is (= src (elide/without-protocol-docstrings src))
        "def docstrings are the compiler's job; doing it twice would diverge")))

(deftest a-defprotocol-named-inside-a-string-is-not-a-form
  (let [src "(def doc \"see (defprotocol IRank (rank [this] \\\"x\\\"))\")\n"]
    (is (empty? (elide/protocol-docstring-spans src)))))

(deftest a-defprotocol-in-a-comment-is-not-a-form
  (let [src ";; (defprotocol IRank (rank [this] \"the secret\"))\n(def x 1)\n"]
    (is (empty? (elide/protocol-docstring-spans src)))))

(deftest a-nested-defprotocol-is-still-reached
  (testing "a reader conditional or do block does not hide it"
    (let [src "(do (defprotocol IRank (rank [this] \"Nested but still shipped.\")))\n"]
      (is (= 1 (count (elide/protocol-docstring-spans src))))
      (is (not (re-find #"still shipped" (elide/without-protocol-docstrings src)))))))

;; ── both together ─────────────────────────────────────────────────────────

(deftest without-docstrings-removes-both-classes
  (let [src (str "(ns acme.core\n  \"Why this namespace exists.\")\n"
                 "(defprotocol IRank (rank [this] \"How it ranks.\"))\n")
        out (elide/without-docstrings src)]
    (is (not (re-find #"Why this namespace" out)))
    (is (not (re-find #"How it ranks" out)))
    (is (= (count (reads-as src)) (count (reads-as out))))))

(deftest elision-is-deletion-only
  (testing "the result is a subsequence of the input, so nothing is rewritten"
    (let [src (str "(ns acme.core \"A.\" (:require [clojure.string :as str]))\n"
                   "(defprotocol I (m [this] \"B.\") (n [this]))\n"
                   "(defn f \"C.\" [] (str \"literal (\" 1 \"))\"))\n")
          out (elide/without-docstrings src)
          subsequence? (fn [^String o ^String s]
                         (loop [i 0 j 0]
                           (cond (>= i (count o)) true
                                 (>= j (count s)) false
                                 (= (.charAt o i) (.charAt s j)) (recur (inc i) (inc j))
                                 :else (recur i (inc j)))))]
      (is (subsequence? out src))
      (testing "a string literal in the body is left alone"
        (is (re-find #"literal" out))))))

(deftest clojure-source-recognises-the-dialects-the-pass-rewrites
  (is (elide/clojure-source? "a/b.clj"))
  (is (elide/clojure-source? "a/b.cljc"))
  (is (elide/clojure-source? "a/b.cljs"))
  (is (not (elide/clojure-source? "a/b.edn")))
  (is (not (elide/clojure-source? "a/b.class"))))
