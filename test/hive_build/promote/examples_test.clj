(ns hive-build.promote.examples-test
  "The README-as-claims layer. Pure, so the schemas synthesize most of it."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-build.promote.examples :as ex]
            [hive-schemas.test :as st]
            [malli.generator :as mg]))

;; ── Synthesized from the schemas ───────────────────────────────────────────

(def Markdown
  "A document rendered from generated blocks, so the extractor meets documents
   that actually hold fences rather than random text that never does."
  [:string {:gen/gen (gen/fmap ex/markdown (mg/generator [:vector {:gen/max 4} ex/Block]))}])

(st/deftrifecta-from-schema blocks
  hive-build.promote.examples/blocks
  {:in  Markdown
   :out [:sequential ex/Block]
   :rel (fn [in out]
          (and (= (range (count out)) (map :block/index out))
               (apply < 0 (map :block/line out))
               (every? (fn [{:block/keys [lang info source run?]}]
                         (and (str/includes? in source)
                              (str/includes? in info)
                              (= lang (ex/lang-of info))
                              (= run? (ex/runnable? lang info))))
                       out)))
   :num-tests 100})

(st/deftrifecta-from-schema examples
  hive-build.promote.examples/examples
  {:in  ex/Block
   :out [:maybe [:sequential ex/Example]]
   :rel (fn [{:block/keys [index run?] src :block/source} out]
          (if run?
            (and (seq out)
                 (every? (fn [{:example/keys [block source expected]}]
                           (and (= block index)
                                (str/includes? src source)
                                (or (nil? expected) (str/includes? src expected))))
                         out))
            (nil? out)))
   :num-tests 100})

(st/deftrifecta-from-schema script
  hive-build.promote.examples/script
  {:in  [:sequential {:gen/max 4} ex/Block]
   :out :string
   :rel (fn [in out]
          (and (every? #(str/includes? out (:block/source %)) (filter :block/run? in))
               (str/ends-with? out "(user/readme-examples--exit)\n")))
   ;; scalar output, no map entries to corrupt
   :mutation false
   :num-tests 50})

(st/deftrifecta-from-schema summary
  hive-build.promote.examples/summary
  {:in  [:sequential {:gen/max 4} ex/Block]
   :out ex/Summary
   :rel (fn [in {:examples/keys [blocks runnable skipped]}]
          (and (= blocks (count in))
               (= blocks (+ runnable skipped))
               (= runnable (count (filter :block/run? in)))))
   :num-tests 50})

;; ── What a schema cannot state ────────────────────────────────────────────

(def ^:private readme
  (str/join "\n"
            ["# lib" ""
             "```clojure"
             "(require '[clojure.string :as str])"
             "(+ 1 2)"
             ";; => 3"
             ""
             "(str/join \",\" [1 2])"
             ""
             ";; => \"1,2\""
             "(def x"
             "  1)"
             "```"
             "prose"
             "```sh"
             "clojure -M:test"
             "```"
             "```clojure no-run"
             "(System/exit 9)"
             "```"
             "```clj"
             "(+ 1"
             "```"]))

(deftest every-fence-is-a-block-and-only-clojure-fences-run
  (let [bs (ex/blocks readme)]
    (is (= ["clojure" "sh" "clojure" "clj"] (map :block/lang bs)))
    (is (= [true false false true] (map :block/run? bs)))
    (is (= [3 15 18 21] (map :block/line bs)) "the line of each opening fence")
    (is (= (range 4) (map :block/index bs)))))

(deftest a-claim-follows-its-form-across-blank-lines-and-not-otherwise
  (let [[b] (ex/blocks readme)
        exs (ex/examples b)]
    (is (= ["(require '[clojure.string :as str])" "(+ 1 2)" "(str/join \",\" [1 2])" "(def x\n  1)"]
           (map :example/source exs))
        "each form verbatim, multi-line ones included")
    (is (= [nil "3" "\"1,2\"" nil] (map :example/expected exs)))
    (is (= [1 2 5 8] (map :example/line exs)) "lines within the block")))

(deftest a-block-the-reader-cannot-take-apart-is-one-example-with-no-claim
  (is (= [{:example/block 3 :example/line 1 :example/source "(+ 1" :example/expected nil}]
         (ex/examples (last (ex/blocks readme))))
      "the cold JVM reports the real reader error, not this layer"))

(deftest every-form-kind-is-sliced-verbatim-and-a-claim-goes-to-the-last-form-on-its-line
  (let [[b] (ex/blocks (str "```clojure\n"
                            ";; a deps.edn fragment, then two forms on one line\n"
                            ":build {:deps {a 1}\n"
                            "        :jvm-opts [\"x\"]}\n"
                            "(foo) bar\n"
                            ";; => 1\n"
                            "  \"str\" 42\n"
                            "\n"
                            ";; => 42\n"
                            "```"))
        exs (ex/examples b)]
    (is (= [":build" "{:deps {a 1}\n        :jvm-opts [\"x\"]}" "(foo)" "bar" "\"str\"" "42"]
           (map :example/source exs))
        "keywords, maps and bare tokens are sliced exactly, not only lists")
    (is (= [2 2 4 4 6 6] (map :example/line exs)))
    (is (= [nil nil nil "1" nil "42"] (map :example/expected exs))
        "the claim under (foo) bar belongs to bar, the one under 42 to 42")))

(deftest a-block-kept-out-of-the-run-has-no-examples
  (let [bs (ex/blocks readme)]
    (testing "a fence in another language"
      (is (nil? (ex/examples (nth bs 1)))))
    (testing "a clojure fence marked no-run"
      (is (nil? (ex/examples (nth bs 2)))))))

(deftest an-unterminated-fence-is-not-a-block
  (is (= [] (ex/blocks "```clojure\n(+ 1 2)")))
  (is (= 1 (count (ex/blocks "```clojure\n(+ 1 2)\n```\n```clojure\n(open")))))

(deftest nothing-and-nil-hold-no-blocks
  (is (= [] (ex/blocks nil)))
  (is (= [] (ex/blocks ""))))

(deftest the-summary-counts-what-was-claimed
  (is (= {:examples/blocks 4 :examples/runnable 2 :examples/skipped 2 :examples/claims 2}
         (ex/summary (ex/blocks readme)))))

(deftest the-script-names-each-form-and-wraps-only-the-claimed-ones
  (let [s (ex/script (ex/blocks readme))]
    (is (str/includes? s "(user/readme-examples--at 0 1)\n(require '[clojure.string :as str])"))
    (is (str/includes? s "(user/readme-examples--check 0 2 (do\n(+ 1 2)\n) \"3\")"))
    (is (str/includes? s "(user/readme-examples--check 0 5 (do\n(str/join \",\" [1 2])\n) \"\\\"1,2\\\"\")"))
    (is (not (str/includes? s "System/exit 9")) "a no-run block never reaches the program")
    (is (str/includes? s "(user/readme-examples--at 3 1)\n(+ 1") "an unreadable block still runs, and fails there")
    (is (str/ends-with? s "(user/readme-examples--exit)\n"))))

(deftest markdown-is-the-inverse-of-blocks-for-sources
  (let [bs [{:block/index 0 :block/line 1 :block/lang "clojure" :block/info "clojure"
             :block/source "(+ 1 2)\n;; => 3" :block/run? true}
            {:block/index 1 :block/line 1 :block/lang "" :block/info ""
             :block/source "" :block/run? false}
            {:block/index 2 :block/line 1 :block/lang "clojure" :block/info "clojure no-run"
             :block/source "a\n\nb\n" :block/run? false}]]
    (is (= (map :block/source bs) (map :block/source (ex/blocks (ex/markdown bs)))))
    (is (= (map :block/run? bs) (map :block/run? (ex/blocks (ex/markdown bs)))))))
