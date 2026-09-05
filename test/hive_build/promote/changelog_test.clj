(ns hive-build.promote.changelog-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-schemas.test :as st]
            [hive-build.promote.changelog :as changelog]
            [hive-build.promote.notes :as notes]))

;; ── Synthesized from the schemas ───────────────────────────────────────────

(st/deftrifecta-from-schema render
  hive-build.promote.changelog/render
  {:in changelog/Changelog
   :out :string
   ;; Every release the document was given has to be findable in it. A renderer
   ;; that silently drops one is the failure this whole change exists to avoid.
   :rel (fn [in out]
          (every? #(str/includes? out (:release/version %))
                  (:changelog/releases in)))
   ;; scalar output, no map entries to corrupt
   :mutation false
   :num-tests 50})

;; ── What a schema cannot state ────────────────────────────────────────────

(def ^:private note-with
  {:note/sections [{:section/heading "Added"
                    :section/kind "feat"
                    :section/entries [{:note/kind "feat" :note/scope "api"
                                       :note/breaking? false
                                       :note/summary "a new task"}]}]
   :note/breaking [{:note/kind "feat" :note/scope nil
                    :note/breaking? true
                    :note/summary "the port grew a method"}]
   :note/commits 9
   :note/routine 7
   :note/unconventional 1})

(def ^:private note-quiet
  {:note/sections [] :note/breaking []
   :note/commits 3 :note/routine 3 :note/unconventional 0})

(deftest authored-prose-is-spliced-under-its-heading
  (let [out (changelog/render
             {:changelog/preamble nil
              :changelog/footer nil
              :changelog/releases [{:release/version "1.0.0"
                                    :release/date "2026-09-05"
                                    :release/prose "Adding a method to a protocol is major."
                                    :release/note note-quiet}]})]
    (is (str/includes? out "## [1.0.0] - 2026-09-05"))
    (is (str/includes? out "Adding a method to a protocol is major.")
        "prose a generator cannot derive has to survive the render")
    (is (< (str/index-of out "## [1.0.0]")
           (str/index-of out "Adding a method"))
        "and it belongs under its own heading, not above it")))

(deftest breaking-changes-outrank-everything-else
  (let [out (changelog/render
             {:changelog/preamble nil :changelog/footer nil
              :changelog/releases [{:release/version "2.0.0" :release/date nil
                                    :release/prose nil :release/note note-with}]})]
    (is (< (str/index-of out "### BREAKING") (str/index-of out "### Added"))
        "a reader deciding whether to bump has one question that outranks the rest")
    (is (str/includes? out "the port grew a method"))))

(deftest scope-leads-the-line-when-a-commit-has-one
  (is (= "- **api:** a new task"
         (changelog/entry-line {:note/kind "feat" :note/scope "api"
                                :note/breaking? false :note/summary "a new task"})))
  (is (= "- a new task"
         (changelog/entry-line {:note/kind "feat" :note/scope nil
                                :note/breaking? false :note/summary "a new task"}))))

(deftest housekeeping-is-reported-rather-than-hidden
  (testing "a release of pure chores reads as one, not as an empty release"
    (let [out (changelog/render
               {:changelog/preamble nil :changelog/footer nil
                :changelog/releases [{:release/version "0.1.10" :release/date nil
                                      :release/prose nil :release/note note-quiet}]})]
      (is (str/includes? out "_No user-facing changes._"))
      (is (str/includes? out "3 routine commits"))
      (is (false? (notes/newsworthy? note-quiet)))))
  (testing "the count includes subjects that follow no convention"
    (is (str/includes? (changelog/routine-line note-with) "8 routine commits"))))

(deftest an-unreleased-repository-still-has-a-changelog
  (let [out (changelog/render {:changelog/preamble nil :changelog/footer nil
                               :changelog/releases []})]
    (is (str/includes? out "# Changelog"))
    (is (str/includes? out "GENERATED")
        "the file has to say it is generated, or somebody will edit it")))

(deftest a-supplied-preamble-replaces-the-generated-one
  (let [out (changelog/render {:changelog/preamble "# Notes\n\nWhat the version promises."
                               :changelog/footer "_older tags elsewhere_"
                               :changelog/releases []})]
    (is (str/starts-with? out "# Notes"))
    (is (not (str/includes? out "GENERATED")))
    (is (str/includes? out "_older tags elsewhere_"))))
