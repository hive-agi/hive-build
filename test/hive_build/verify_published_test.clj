(ns hive-build.verify-published-test
  "The post-deploy effect check: a release run is green only when the
   coordinate it claims to have published is in the registry."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.test :as st]
            [hive-build.boundary.tools :as tools]
            [hive-build.promote.project :as project]
            [hive-build.promote.publish :as publish]))

;; ── Synthesized from the schemas ──────────────────────────────────────────

(st/deftrifecta-from-schema verdict
  hive-build.promote.publish/verdict
  {:in publish/VerifyInput
   :out publish/Verdict
   ;; Only a complete coordinate may pass, and the verdict reports the state
   ;; it was given rather than a reinterpretation of it.
   :rel (fn [in out]
          (and (= (:verdict/ok? out) (= :complete (:verify/state in)))
               (= (:verdict/state out) (:verify/state in))
               (.contains ^String (:verdict/message out) (:verify/label in))))
   :num-tests 100})

;; ── Who may be probed, and how ────────────────────────────────────────────

(def ci-env {"MAVEN_URL" "https://gitea.test/maven"
             "MAVEN_USERNAME" "bot"
             "MAVEN_TOKEN" "tok"})

(deftest a-private-registry-is-probed-with-credentials-or-not-at-all
  (testing "an anonymous probe answers 401, which reads exactly like `missing`"
    (is (= {:probe/skip :no-credentials}
           (publish/probe-auth (publish/target :gitea) {"MAVEN_URL" "https://gitea.test/maven"}))))
  (testing "with credentials the probe authenticates"
    (let [{:probe/keys [url auth]} (publish/probe-auth (publish/target :gitea) ci-env)]
      (is (= "https://gitea.test/maven" url))
      (is (= (publish/basic-auth "bot" "tok") auth))))
  (testing "a target with no URL cannot be probed"
    (is (= {:probe/skip :no-url} (publish/probe-auth (publish/target :gitea) {})))))

(deftest a-public-registry-is-probed-anonymously
  (is (= {:probe/url "https://repo.clojars.org" :probe/auth nil}
         (publish/probe-auth (publish/target :clojars) {}))))

;; ── The check, driven through its ports ───────────────────────────────────

(defn- project-for [target-id]
  (project/project {:lib 'io.github.hive-agi/hive-thing :publish target-id} "1.2.3"))

(defn- head-stub
  "A registry that answers `pom` for the pom and `jar` for the jar, recording
   every URL and Authorization header it was asked about."
  [pom jar calls]
  (fn [url auth]
    (swap! calls conj [url auth])
    (if (.endsWith ^String url ".pom") pom jar)))

(defn- outcome [target-id env pom jar]
  (let [calls (atom [])
        result (try (with-out-str
                      (tools/verify-published! (project-for target-id)
                                               {:env env :head-fn (head-stub pom jar calls)}))
                    :ok
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    {:result result :calls @calls}))

(deftest a-complete-coordinate-passes
  (let [{:keys [result calls]} (outcome :gitea ci-env :present :present)]
    (is (= :ok result))
    (is (= 2 (count calls)) "both the pom and the jar are probed")
    (is (every? #(= (publish/basic-auth "bot" "tok") (second %)) calls))
    (is (= "https://gitea.test/maven/io/github/hive-agi/hive-thing/1.2.3/hive-thing-1.2.3.pom"
           (ffirst calls)))))

(deftest anything-short-of-complete-fails-the-run
  (doseq [[pom jar state] [[:absent :absent :absent]
                           [:present :absent :partial]
                           [:unreachable :present :unknown]]]
    (let [{:keys [result]} (outcome :gitea ci-env pom jar)]
      (is (= {:reason :release/not-published :state state} result)
          (str pom "/" jar " must not pass")))))

(deftest a-check-that-cannot-look-fails-rather-than-passing
  (testing "missing secrets: the deploy had none either, so nothing was published"
    (let [{:keys [result calls]} (outcome :gitea {"MAVEN_URL" "https://gitea.test/maven"}
                                          :present :present)]
      (is (= :no-credentials (:state result)))
      (is (empty? calls) "no anonymous probe is ever made of a private registry"))))

(deftest a-package-that-does-not-ship-has-nothing-to-verify
  (let [{:keys [result calls]} (outcome :none {} :absent :absent)]
    (is (= :ok result))
    (is (empty? calls))))
