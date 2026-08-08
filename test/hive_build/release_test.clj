(ns hive-build.release-test
  "A whole release, end to end, with no filesystem, no network and no jar.

   The plan is a value and the runner takes its handlers as an argument, so
   every task can be driven from version.edn to publish request and the exact
   sequence of effects asserted."
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [hive-build.boundary.run :as run]
            [hive-build.boundary.tools :as tools]
            [hive-build.pipeline.plan :as plan]
            [hive-build.promote.project :as project]
            [hive-build.promote.publish :as publish]
            [hive-build.schema :as s]))

;; ── The registry must cover the schema ────────────────────────────────────

(defn step-kinds
  "Every :step/kind the Step schema admits."
  []
  (into #{} (map first) (m/children (m/schema s/Step))))

(deftest every-step-the-schema-allows-has-a-handler
  (testing "adding a Step variant without a runner would produce a plan that
            passes validation and dies halfway through a release"
    (is (= (step-kinds) (set (keys tools/handlers))))))

(deftest every-plan-of-every-task-is-runnable
  (doseq [target-id (publish/target-ids)
          task [:task/clean :task/jar :task/jar-aot :task/install :task/deploy]
          published? [true false]]
    (let [p (plan/plan task
                       (project/project {:lib 'io.github.hive-agi/hive-thing
                                         :publish target-id}
                                        "1.2.3")
                       {:facts/source-roots ["src"]
                        :facts/resource-roots []
                        :facts/namespaces ['hive-thing.core]
                        :facts/preload []
                        :facts/published? published?})]
      (is (run/runnable? tools/handlers p)
          (str task " / " target-id " / published? " published?)))))

;; ── A release, driven end to end ──────────────────────────────────────────

(def cfg
  {:lib 'io.github.hive-agi/hive-thing
   :license {:name "MIT" :url "https://opensource.org/licenses/MIT"}
   :scm-url "https://github.com/hive-agi/hive-thing"
   :src-dirs ["src" "resources"]
   :publish :gitea})

(def facts
  {:facts/source-roots ["src"]
   :facts/resource-roots ["resources"]
   :facts/namespaces ['hive-thing.core 'hive-thing.impl]
   :facts/preload ['host.protocol]
   :facts/published? false})

(defn trace
  "Run `task` against handlers that record instead of act. Returns the effect
   trace, plus whatever the publish step was asked to send."
  [task cfg' facts']
  (let [log (atom [])
        published (atom nil)
        p (project/project cfg' "1.2.3")
        handlers (into {} (map (fn [k] [k (fn [_ctx step] (swap! log conj step) k)]))
                       (keys tools/handlers))
        handlers (assoc handlers
                        :step/publish
                        (fn [ctx step]
                          (swap! log conj step)
                          (let [target (publish/target (:step/target-id step))]
                            (reset! published
                                    (publish/deploy-request
                                     target
                                     {:artifact (get-in ctx [:ctx/project :project/jar-file])
                                      :pom-file "target/classes/META-INF/maven/pom.xml"
                                      :installer (:step/installer step)
                                      :env {"MAVEN_URL" "https://gitea.test/maven"
                                            "MAVEN_USERNAME" "bot"
                                            "MAVEN_TOKEN" "tok"}})))))]
    (run/run! handlers {:ctx/project p} (plan/plan task p facts'))
    {:steps @log :published @published :project p}))

(deftest a-private-release-ships-classes-and-no-source
  (let [{:keys [steps published]} (trace :task/deploy cfg facts)]
    (testing "the effects, in order"
      (is (= [:step/clean :step/compile :step/copy-classes :step/copy-dir
              :step/write-pom :step/jar :step/normalize :step/announce
              :step/publish :step/announce]
             (mapv :step/kind steps))))
    (testing "only resources are copied — no source directory is packaged"
      (is (= ["resources"] (:step/src-dirs (nth steps 3)))))
    (testing "the host namespace compiles but is not packaged"
      (is (= ['host.protocol 'hive-thing.core 'hive-thing.impl]
             (:step/ns-compile (nth steps 1))))
      (is (= ["hive_thing/core" "hive_thing/impl"] (:step/prefixes (nth steps 2)))))
    (testing "and the artifact reaches the private registry with its own credentials"
      (is (= {:installer :remote
              :artifact "target/hive-thing-1.2.3.jar"
              :pom-file "target/classes/META-INF/maven/pom.xml"
              :repository {"gitea" {:url "https://gitea.test/maven"
                                    :username "bot"
                                    :password "tok"}}}
             published)))))

(deftest a-public-release-ships-source
  (let [{:keys [steps published]} (trace :task/deploy (assoc cfg :publish :clojars) facts)]
    (is (= [:step/clean :step/write-pom :step/copy-dir :step/jar :step/normalize
            :step/announce :step/publish :step/announce]
           (mapv :step/kind steps)))
    (is (= ["src" "resources"] (:step/src-dirs (nth steps 2))))
    (is (= :remote (:installer published)))
    (is (not (contains? published :repository)))))

(deftest a-package-that-does-not-ship-touches-nothing
  (let [{:keys [steps published]} (trace :task/deploy (assoc cfg :publish :none) facts)]
    (is (= [:step/announce] (mapv :step/kind steps)))
    (is (nil? published))))

(deftest re-releasing-a-published-coordinate-touches-nothing
  (let [{:keys [steps published]} (trace :task/deploy cfg (assoc facts :facts/published? true))]
    (is (= [:step/announce] (mapv :step/kind steps)))
    (is (nil? published))))

(deftest an-install-never-carries-credentials
  (let [{:keys [published]} (trace :task/install cfg facts)]
    (is (= :local (:installer published)))
    (is (not (contains? published :repository)))))

(deftest the-jar-that-is-published-is-the-jar-that-was-built
  (doseq [target-id [:clojars :gitea :gitea-source]]
    (let [{:keys [steps published project]} (trace :task/deploy (assoc cfg :publish target-id) facts)
          jar-step (first (filter #(= :step/jar (:step/kind %)) steps))]
      (is (= (:project/jar-file project) (:step/jar-file jar-step)) (str target-id))
      (is (= (:step/jar-file jar-step) (:artifact published)) (str target-id)))))

(deftest a-version-bump-changes-the-artifact-that-is-published
  (testing "the coordinate is what distinguishes one release from the next"
    (let [jar-of (fn [version]
                   (:project/jar-file (project/project cfg version)))]
      (is (= "target/hive-thing-1.2.3.jar" (jar-of "1.2.3")))
      (is (= "target/hive-thing-1.2.4.jar" (jar-of "1.2.4")))
      (is (= (jar-of "1.2.3") (:artifact (:published (trace :task/deploy cfg facts))))))))
