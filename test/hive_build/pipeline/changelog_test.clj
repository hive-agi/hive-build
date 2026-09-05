(ns hive-build.pipeline.changelog-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-build.pipeline.changelog :as plan]))

(def ^:private tags ["v1.2.0" "v1.1.0" "v1.0.0"])

(deftest ranges-pair-each-tag-with-the-one-before-it
  (is (= [{:release/version "1.2.0" :range/to "v1.2.0" :range/from "v1.1.0"}
          {:release/version "1.1.0" :range/to "v1.1.0" :range/from "v1.0.0"}
          {:release/version "1.0.0" :range/to "v1.0.0" :range/from nil}]
         (plan/ranges tags)))
  (testing "the oldest release opens at the root of history"
    (is (nil? (:range/from (last (plan/ranges tags)))))))

(deftest depth-truncates-the-list-and-never-widens-a-range
  ;; The bug this guards: truncating the TAGS before pairing would leave the
  ;; oldest shown release with :range/from nil, and attribute the entire
  ;; history to it.
  (let [doc (plan/document tags {:depth 2
                                 :messages-fn (fn [from to] [(str "feat: " from "->" to)])
                                 :date-fn (constantly "2026-09-05")
                                 :prose-fn (constantly nil)})
        releases (:changelog/releases doc)
        summaries (mapcat (fn [r] (map :note/summary
                                       (mapcat :section/entries
                                               (get-in r [:release/note :note/sections]))))
                          releases)]
    (is (= 2 (count releases)))
    (is (= ["1.2.0" "1.1.0"] (mapv :release/version releases)))
    ;; the summary is what follows the conventional-commit kind, so the "feat: "
    ;; prefix is consumed by the parse and never reaches the note
    (is (= ["v1.1.0->v1.2.0" "v1.0.0->v1.1.0"] (vec summaries))
        "the second release still starts at v1.0.0, not at the root")))

(deftest releases-left-out-are-named-rather-than-dropped
  (let [doc (plan/document tags {:depth 1
                                 :messages-fn (constantly [])
                                 :date-fn (constantly nil)
                                 :prose-fn (constantly nil)})]
    (is (= 1 (count (:changelog/releases doc))))
    (is (re-find #"2 older releases" (:changelog/footer doc))))
  (testing "and nothing is said when nothing was left out"
    (is (nil? (:changelog/footer (plan/document tags {:messages-fn (constantly [])
                                                      :date-fn (constantly nil)
                                                      :prose-fn (constantly nil)}))))))

(deftest every-read-is-injected
  ;; DIP: the assembly runs against maps here and against git in a release,
  ;; with no branch anywhere that knows which one it is.
  (let [asked (atom [])
        doc (plan/document ["v0.2.0" "v0.1.0"]
                           {:messages-fn (fn [from to] (swap! asked conj [from to]) [])
                            :date-fn (constantly "2026-01-01")
                            :prose-fn (fn [version] (str "prose for " version))})]
    (is (= [["v0.1.0" "v0.2.0"] [nil "v0.1.0"]] @asked))
    (is (= ["prose for 0.2.0" "prose for 0.1.0"]
           (mapv :release/prose (:changelog/releases doc))))
    (is (= ["2026-01-01" "2026-01-01"]
           (mapv :release/date (:changelog/releases doc))))))

(deftest a-repository-that-has-never-tagged-renders-nothing-and-does-not-throw
  (let [doc (plan/document [] {:messages-fn (constantly [])
                               :date-fn (constantly nil)
                               :prose-fn (constantly nil)})]
    (is (= [] (:changelog/releases doc)))
    (is (nil? (:changelog/footer doc)))))

(deftest a-pending-release-describes-itself-before-it-is-tagged
  ;; The staleness this guards: a changelog regenerated during a release, but
  ;; before the tag exists, would describe every version except the one being
  ;; released.
  (let [asked (atom [])
        doc (plan/document tags {:pending "1.3.0"
                                 :messages-fn (fn [from to] (swap! asked conj [from to]) [])
                                 :date-fn (constantly "2026-09-05")
                                 :prose-fn (constantly nil)})]
    (is (= "1.3.0" (:release/version (first (:changelog/releases doc)))))
    (is (= ["v1.2.0" "HEAD"] (first @asked))
        "its range runs from the newest tag to HEAD")
    (is (= 4 (count (:changelog/releases doc)))
        "and the tagged releases are all still there"))
  (testing "a v-prefixed pending version is normalised like any tag"
    (is (= "1.3.0" (-> (plan/document tags {:pending "v1.3.0"
                                            :messages-fn (constantly [])
                                            :date-fn (constantly nil)
                                            :prose-fn (constantly nil)})
                       :changelog/releases first :release/version))))
  (testing "no pending version leaves the document as the tags describe it"
    (is (= 3 (count (:changelog/releases
                     (plan/document tags {:pending nil
                                          :messages-fn (constantly [])
                                          :date-fn (constantly nil)
                                          :prose-fn (constantly nil)})))))))

(deftest a-first-release-has-no-previous-tag-to-open-at
  (let [asked (atom [])]
    (plan/document [] {:pending "0.1.0"
                       :messages-fn (fn [from to] (swap! asked conj [from to]) [])
                       :date-fn (constantly nil)
                       :prose-fn (constantly nil)})
    (is (= [[nil "HEAD"]] @asked)
        "the first release reads from the root of history")))

(deftest version-drops-the-tag-prefix-and-only-that
  (is (= "1.2.3" (plan/version-of "v1.2.3")))
  (is (= "1.2.3" (plan/version-of "1.2.3")))
  (is (= "1.2.3-rc.1" (plan/version-of "v1.2.3-rc.1"))))
