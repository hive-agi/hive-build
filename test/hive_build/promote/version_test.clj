(ns hive-build.promote.version-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.test :as st]
            [hive-build.promote.version :as version]
            [hive-build.schema :as s]))

;; ── Synthesized from the schemas ───────────────────────────────────────────

(st/deftrifecta-from-schema parse
  hive-build.promote.version/parse
  {:in s/VersionString
   :out s/SemVer
   :rel (fn [in out] (= in (version/render out)))
   :num-tests 200})

(st/deftrifecta-from-schema render
  hive-build.promote.version/render
  {:in s/SemVer
   :out s/VersionString
   :rel (fn [in out] (= in (version/parse out)))
   ;; scalar output — no map entries to corrupt
   :mutation false
   :num-tests 200})

(st/deftrifecta-from-schema bump
  hive-build.promote.version/bump
  {:in [:cat s/SemVer s/BumpLevel]
   :out s/SemVer
   ;; The invariant a release depends on: a bump always moves forward.
   :rel (fn [[semver _level] out] (pos? (version/compare-versions out semver)))
   :num-tests 300})

(st/deftrifecta-from-schema next-version
  hive-build.promote.version/next-version
  {:in [:cat s/VersionString s/BumpLevel]
   :out s/VersionString
   :rel (fn [[text _level] out]
          (pos? (version/compare-versions (version/parse out) (version/parse text))))
   :mutation false
   :num-tests 200})

;; ── What a schema cannot state ────────────────────────────────────────────

(deftest bump-touches-only-its-level-and-below
  (let [v {:semver/major 2 :semver/minor 7 :semver/patch 5}]
    (testing ":patch leaves major and minor alone"
      (is (= {:semver/major 2 :semver/minor 7 :semver/patch 6} (version/bump v :patch))))
    (testing ":minor resets patch"
      (is (= {:semver/major 2 :semver/minor 8 :semver/patch 0} (version/bump v :minor))))
    (testing ":major resets minor and patch"
      (is (= {:semver/major 3 :semver/minor 0 :semver/patch 0} (version/bump v :major))))))

(deftest bump-refuses-an-unknown-level
  (is (thrown? clojure.lang.ExceptionInfo
               (version/bump {:semver/major 1 :semver/minor 0 :semver/patch 0} :epoch))))

(deftest parse-rejects-what-is-not-a-version
  (testing "nil rather than a partial parse, so callers cannot proceed on a guess"
    (doseq [text ["" "1" "1.2" "1.2.3.4" "v1.2.3" "1.2.x" "one.two.three"
                  "1.-2.3" " " nil 123]]
      (is (nil? (version/parse text)) (str "parsed " (pr-str text))))))

(deftest parse-tolerates-surrounding-whitespace
  (testing "a VERSION file ends with a newline"
    (is (= {:semver/major 0 :semver/minor 3 :semver/patch 10}
           (version/parse "0.3.10\n")))))

(deftest next-version-refuses-to-guess
  (testing "an unparseable VERSION stops the release rather than inventing one"
    (is (thrown? clojure.lang.ExceptionInfo (version/next-version "not-a-version" :patch)))
    (is (thrown? clojure.lang.ExceptionInfo (version/next-version "" :patch)))))

(deftest ordering-is-by-component-not-by-string
  (testing "10 is greater than 9, which string comparison gets wrong"
    (is (pos? (version/compare-versions (version/parse "0.10.0") (version/parse "0.9.0"))))
    (is (pos? (version/compare-versions (version/parse "1.0.0") (version/parse "0.999.999"))))
    (is (zero? (version/compare-versions (version/parse "1.2.3") (version/parse "1.2.3"))))))

(deftest a-release-never-reuses-a-coordinate
  (testing "repeated bumps are strictly increasing at every level"
    (doseq [level [:major :minor :patch]]
      (let [versions (take 25 (iterate #(version/next-version % level) "0.0.0"))]
        (is (= versions (distinct versions)))
        (is (every? (fn [[a b]]
                      (pos? (version/compare-versions (version/parse b) (version/parse a))))
                    (partition 2 1 versions)))))))
