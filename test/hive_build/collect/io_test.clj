(ns hive-build.collect.io-test
  (:require [clojure.java.io :as jio]
            [clojure.test :refer [deftest is testing]]
            [hive-build.collect.io :as io'])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (str (Files/createTempDirectory "hive-build-io" (make-array FileAttribute 0))))

(deftest an-absent-file-reads-as-nil
  (let [dir (temp-dir)]
    (is (nil? (io'/read-text (str dir "/missing.edn"))))
    (is (nil? (io'/read-edn (str dir "/missing.edn"))))
    (is (not (io'/exists? (str dir "/missing.edn"))))))

(deftest a-directory-is-not-a-readable-file
  (let [dir (temp-dir)]
    (is (nil? (io'/read-text dir)))))

(deftest edn-round-trips
  (let [path (str (temp-dir) "/x.edn")]
    (io'/write-text! path (pr-str {:lib 'g/a :publish :clojars}))
    (is (= {:lib 'g/a :publish :clojars} (io'/read-edn path)))))

(deftest a-parse-failure-names-the-file
  (testing "the underlying message says what was wrong, never where, and a
            build reads version.edn, deps.edn and local.deps.edn"
    (let [path (str (temp-dir) "/local.deps.edn")]
      (io'/write-text! path "{:deps {a/b {} a/b {}}}")
      (let [e (try (io'/read-edn path) nil (catch clojure.lang.ExceptionInfo e e))]
        (is (some? e) "malformed EDN must not read as nil")
        (is (= path (:path (ex-data e))))
        (is (re-find #"could not parse" (ex-message e)))
        (is (re-find #"local\.deps\.edn" (ex-message e)))
        (is (re-find #"Duplicate key" (ex-message e)))))))

(deftest declared-ns-finds-the-namespace-past-a-leading-comment
  (let [path (str (temp-dir) "/a.clj")]
    (io'/write-text! path ";; a header comment\n;; and another\n(ns some.thing.core)\n(defn f [])")
    (is (= 'some.thing.core (io'/declared-ns path)))))

(deftest declared-ns-is-nil-for-a-file-with-no-ns-form
  (let [path (str (temp-dir) "/none.clj")]
    (io'/write-text! path ";; just a comment\n42\n")
    (is (nil? (io'/declared-ns path)))))

(deftest spdx-ids-collects-distinct-identifiers
  (let [dir (temp-dir)
        a (str dir "/a.clj") b (str dir "/b.clj") c (str dir "/c.clj")]
    (io'/write-text! a ";; SPDX-License-Identifier: MIT\n")
    (io'/write-text! b ";; SPDX-License-Identifier: MIT\n")
    (io'/write-text! c ";; SPDX-License-Identifier: AGPL-3.0-only\n")
    (is (= #{"MIT" "AGPL-3.0-only"} (io'/spdx-ids [a b c])))
    (is (= #{} (io'/spdx-ids [])))))

(deftest an-spdx-line-inside-a-string-does-not-become-a-phantom-conflict
  (testing "the identifier charset stops at the quote; \\S+ would swallow it"
    (let [dir (temp-dir)
          a (str dir "/a.clj") b (str dir "/b.clj")]
      (io'/write-text! a ";; SPDX-License-Identifier: MIT\n")
      (io'/write-text! b "(def s \"SPDX-License-Identifier: MIT\")\n")
      (is (= #{"MIT"} (io'/spdx-ids [a b]))))))

(deftest files-under-lists-only-regular-files
  (let [dir (temp-dir)]
    (jio/make-parents (str dir "/nested/deep/x.clj"))
    (io'/write-text! (str dir "/nested/deep/x.clj") "")
    (io'/write-text! (str dir "/top.clj") "")
    (is (= #{(str dir "/nested/deep/x.clj") (str dir "/top.clj")}
           (set (io'/files-under dir))))))

(deftest files-under-an-absent-directory-is-empty
  (is (= [] (io'/files-under (str (temp-dir) "/nope")))))

(deftest env-refuses-a-blank-credential
  (testing "a deploy that proceeds with a missing credential fails at the
            registry, after the artifact has been built"
    (with-redefs [io'/getenv {"SET" "v" "BLANK" "  " "EMPTY" ""}]
      (is (= {"SET" "v"} (io'/env ["SET"])))
      (is (= {} (io'/env [])))
      (doseq [k ["BLANK" "EMPTY" "ABSENT"]]
        (is (thrown? clojure.lang.ExceptionInfo (io'/env [k])) k)))))

(deftest head-ok-is-false-when-the-registry-is-unreachable
  (testing "an unreachable registry must not read as `already published`,
            which would silently skip a release"
    (is (false? (io'/head-ok? "http://127.0.0.1:1/nope.pom" nil)))
    (is (false? (io'/head-ok? "not a url" nil)))))
