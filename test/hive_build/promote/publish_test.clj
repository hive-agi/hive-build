(ns hive-build.promote.publish-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [malli.core :as m]
            [hive-build.promote.publish :as publish]
            [hive-build.schema :as s]))

(def s3
  "A destination that does not exist in hive, used to prove that adding one
   requires no edit anywhere else."
  {:target/id :s3
   :target/artifact-kind :artifact/aot
   :target/publishes? true
   :target/repo-url nil
   :target/repo-url-env "S3_MAVEN_URL"
   :target/repository-name "s3"
   :target/username-env "S3_KEY_ID"
   :target/password-env "S3_SECRET"})

(use-fixtures :each (fn [t] (try (t) (finally (publish/deregister! :s3)))))

;; ── The registry contract (LSP) ───────────────────────────────────────────

(deftest every-registered-target-satisfies-the-schema
  (doseq [id (publish/target-ids)]
    (is (nil? (m/explain s/Target (publish/target id))) (str "target " id))))

(deftest every-registered-target-answers-the-whole-protocol
  (testing "no branch anywhere may special-case a particular target"
    (doseq [id (publish/target-ids)]
      (let [t (publish/target id)]
        (is (vector? (publish/required-env t)))
        (is (contains? #{:artifact/source :artifact/aot} (:target/artifact-kind t)))
        (is (boolean? (:target/publishes? t)))
        (is (map? (publish/deploy-request t {:artifact "a.jar" :pom-file "pom.xml" :env {}})))))))

(deftest the-four-hive-destinations-are-registered
  (is (= #{:clojars :gitea :gitea-source :none} (publish/target-ids))))

(deftest an-unknown-publish-value-stops-the-release
  (testing "falling through to a default destination would publish a private
            artifact to a public registry"
    (is (thrown? clojure.lang.ExceptionInfo (publish/target :maven-central)))
    (is (thrown? clojure.lang.ExceptionInfo (publish/target nil)))))

(deftest a-new-destination-needs-no-edit-elsewhere
  (publish/register! s3)
  (is (= s3 (publish/target :s3)))
  (is (contains? (publish/target-ids) :s3))
  (is (= ["S3_MAVEN_URL" "S3_KEY_ID" "S3_SECRET"] (publish/required-env (publish/target :s3)))))

;; ── What each destination means ───────────────────────────────────────────

(deftest the-private-registry-ships-no-source
  (is (= :artifact/aot (:target/artifact-kind (publish/target :gitea))))
  (testing ":gitea-source is the deliberate exception, and differs only in that"
    (is (= :artifact/source (:target/artifact-kind (publish/target :gitea-source))))
    (is (= (dissoc (publish/target :gitea) :target/id :target/artifact-kind)
           (dissoc (publish/target :gitea-source) :target/id :target/artifact-kind)))))

(deftest a-package-that-does-not-ship-needs-no-credentials
  (let [t (publish/target :none)]
    (is (false? (:target/publishes? t)))
    (is (= [] (publish/required-env t)))
    (is (nil? (publish/repo-url t {})))))

(deftest required-env-is-derived-from-the-target
  (is (= ["CLOJARS_USERNAME" "CLOJARS_PASSWORD"] (publish/required-env (publish/target :clojars))))
  (is (= ["MAVEN_URL" "MAVEN_USERNAME" "MAVEN_TOKEN"] (publish/required-env (publish/target :gitea)))))

(deftest a-repo-url-comes-from-the-target-or-the-environment-never-a-default
  (is (= "https://repo.clojars.org" (publish/repo-url (publish/target :clojars) {})))
  (is (= "https://gitea.test/maven"
         (publish/repo-url (publish/target :gitea) {"MAVEN_URL" "https://gitea.test/maven"})))
  (testing "an unset MAVEN_URL yields nil rather than a fallback registry"
    (is (nil? (publish/repo-url (publish/target :gitea) {})))))

;; ── The deploy request ────────────────────────────────────────────────────

(deftest a-private-deploy-carries-its-own-credentials
  (is (= {:installer :remote
          :artifact "target/x-1.0.0.jar"
          :pom-file "target/classes/META-INF/maven/g/a/pom.xml"
          :repository {"gitea" {:url "https://gitea.test/maven"
                                :username "bot"
                                :password "tok"}}}
         (publish/deploy-request (publish/target :gitea)
                                 {:artifact "target/x-1.0.0.jar"
                                  :pom-file "target/classes/META-INF/maven/g/a/pom.xml"
                                  :env {"MAVEN_URL" "https://gitea.test/maven"
                                        "MAVEN_USERNAME" "bot"
                                        "MAVEN_TOKEN" "tok"}}))))

(deftest a-clojars-deploy-leaves-the-repository-to-deps-deploy
  (let [request (publish/deploy-request (publish/target :clojars)
                                        {:artifact "a.jar" :pom-file "pom.xml" :env {}})]
    (is (not (contains? request :repository)))
    (is (= :remote (:installer request)))))

(deftest a-local-install-never-carries-a-repository
  (testing "credentials must not travel with an offline install"
    (let [request (publish/deploy-request (publish/target :gitea)
                                          {:artifact "a.jar"
                                           :pom-file "pom.xml"
                                           :installer :local
                                           :env {"MAVEN_URL" "https://gitea.test/maven"
                                                 "MAVEN_USERNAME" "bot"
                                                 "MAVEN_TOKEN" "tok"}})]
      (is (= :local (:installer request)))
      (is (not (contains? request :repository))))))

(deftest basic-auth-encodes-the-standard-header
  (is (= "Basic dXNlcjpwYXNz" (publish/basic-auth "user" "pass"))))

(deftest basic-auth-refuses-a-half-credential
  (testing "an incomplete credential must not become an anonymous request that
            answers 404 and reads as `not yet published`"
    (is (nil? (publish/basic-auth nil "pass")))
    (is (nil? (publish/basic-auth "user" nil)))
    (is (nil? (publish/basic-auth "" "pass")))
    (is (nil? (publish/basic-auth "user" "")))
    (is (nil? (publish/basic-auth nil nil)))))
