(ns hive-build.promote.pom-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [hive-schemas.test :as st]
            [hive-test.mutation :as mut]
            [hive-build.promote.pom :as pom]))

(def Deps [:map-of :symbol :map])

;; ── Synthesized from the schemas ───────────────────────────────────────────

(st/deftrifecta-from-schema prune-deps
  hive-build.promote.pom/prune-deps
  {:in [:cat Deps [:set :symbol]]
   :out Deps
   :rel (fn [[deps exclude] out]
          (and (every? (fn [[k v]] (= v (get deps k))) out)
               (empty? (set/intersection (set (keys out)) exclude))
               (= (set (keys out)) (set/difference (set (keys deps)) exclude))))
   ;; A :map-of output declares no required entries, so the schema can derive
   ;; no mutants. The hand-written ones below carry that burden instead.
   :mutation false
   :num-tests 300})

(st/deftrifecta-from-schema pom-src-dirs
  hive-build.promote.pom/pom-src-dirs
  {:in [:vector [:enum "src" "resources" "src/synth" "dev"]]
   :out [:vector [:string {:min 1}]]
   :rel (fn [in out]
          (and (= out (vec (remove #{"resources"} in)))
               (not (some #{"resources"} out))))
   :mutation false
   :num-tests 200})

;; ── Mutation: the tests above must actually catch a broken prune ──────────

(def ^:private prune-case
  {:deps {'keep/a {:mvn/version "1"}
          'keep/b {:mvn/version "2"}
          'drop/c {:mvn/version "3"}}
   :exclude #{'drop/c}
   :expected {'keep/a {:mvn/version "1"}
              'keep/b {:mvn/version "2"}}})

(mut/deftest-mutations prune-deps-mutations
  hive-build.promote.pom/prune-deps
  [["ships the excluded host lib as a declared requirement"
    (fn [deps _exclude] deps)]
   ["declares no requirements at all"
    (fn [_deps _exclude] {})]
   ["keeps exactly the set that was meant to be dropped"
    (fn [deps exclude] (select-keys deps exclude))]
   ["keeps the right keys but loses their versions"
    (fn [deps exclude] (zipmap (remove exclude (keys deps)) (repeat {})))]]
  (fn []
    (let [{:keys [deps exclude expected]} prune-case]
      (is (= expected (pom/prune-deps deps exclude))))))

;; ── What a schema cannot state ────────────────────────────────────────────

(deftest an-undeclared-licence-is-named-as-such
  (testing "the fallback must not be a real licence: a package with no :license
            must not silently inherit someone else's terms"
    (is (= [[:licenses [:license [:name "UNDECLARED"] [:url ""]]]]
           (pom/pom-data nil)))))

(deftest a-declared-licence-reaches-the-pom-verbatim
  (is (= [[:licenses [:license
                      [:name "MIT"]
                      [:url "https://opensource.org/licenses/MIT"]]]]
         (pom/pom-data {:license/name "MIT"
                        :license/url "https://opensource.org/licenses/MIT"}))))

(deftest a-licence-without-a-url-still-names-itself
  (is (= [[:licenses [:license [:name "MIT"] [:url ""]]]]
         (pom/pom-data {:license/name "MIT" :license/url ""}))))

(deftest excluded-deps-never-reach-the-published-pom
  (testing "host-integration libs are on the compile classpath but are not
            requirements of the artifact"
    (is (= {'a/a {} 'b/b {}}
           (pom/prune-deps {'a/a {} 'b/b {} 'host/x {}} #{'host/x})))
    (is (= {} (pom/prune-deps {'host/x {}} #{'host/x})))))

(deftest excluding-nothing-changes-nothing
  (let [deps {'a/a {:mvn/version "1"} 'b/b {:mvn/version "2"}}]
    (is (= deps (pom/prune-deps deps #{})))))

(deftest excluding-an-absent-dep-is-not-an-error
  (let [deps {'a/a {}}]
    (is (= deps (pom/prune-deps deps #{'never/here})))))

(deftest resources-are-packaged-but-are-not-a-source-root
  (is (= ["src"] (pom/pom-src-dirs ["src" "resources"])))
  (is (= ["src" "src/synth"] (pom/pom-src-dirs ["src" "resources" "src/synth"]))))

(deftest scm-names-the-commit-the-artifact-was-cut-from
  (is (= {:url "https://github.com/hive-agi/hive-build" :tag "deadbeef"}
         (pom/scm "https://github.com/hive-agi/hive-build" "deadbeef"))))
