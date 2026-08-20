(ns hive-build.boundary.tools-test
  "The tools.build adapter. These are wiring assertions: they check that each
   handler hands tools.build the keys tools.build actually reads.

   tools.build destructures its options and ignores anything it does not
   recognise, so a misspelled key is not an error — it is a silently skipped
   feature that reports success. `:compiler-options` instead of `:compile-opts`
   disabled metadata elision on every AOT jar the fleet published, and nothing
   failed."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [clojure.tools.build.api :as b]
            [hive-build.boundary.tools :as tools]
            [hive-build.collect.io :as io']
            [hive-build.promote.project :as project]
            [deps-deploy.maven-settings :as maven-settings]))

(def compile-clj-options
  "Keys clojure.tools.build.api/compile-clj documents. Anything else passed to
   it is discarded without a word."
  #{:basis :class-dir :src-dirs :ns-compile :sort :compile-opts :bindings
    :filter-nses :java-cmd :java-opts :use-cp-file :out :err :out-file :err-file})

(defn- captured-compile
  "The option map the :step/compile handler hands to b/compile-clj."
  [step]
  (let [captured (atom nil)]
    (with-redefs [b/compile-clj (fn [opts] (reset! captured opts) nil)
                  tools/aot-basis (fn [_ _] ::basis)]
      ((get tools/handlers :step/compile) {:ctx/overlay nil} step))
    @captured))

(def compile-step
  {:step/kind :step/compile
   :step/src-dirs ["src"]
   :step/ns-compile ['hive-thing.core]
   :step/class-dir "target/aot-classes"
   :step/elide-meta [:doc :file :line :column :added :arglists]
   :step/java-opts ["-Xmx2g"]})

(deftest metadata-elision-uses-the-key-tools-build-reads
  (testing "the whole S0 opacity rung is this one key being spelled right"
    (let [opts (captured-compile compile-step)]
      (is (= {:elide-meta [:doc :file :line :column :added :arglists]}
             (:compile-opts opts)))
      (is (not (contains? opts :compiler-options))))))

(deftest no-option-is-passed-that-tools-build-would-discard
  (testing "an unrecognised key is a silently skipped feature, not an error"
    (let [opts (captured-compile compile-step)
          unknown (set/difference (set (keys opts)) compile-clj-options)]
      (is (empty? unknown) (str "tools.build will ignore: " unknown)))))

(deftest the-compile-step-carries-its-arguments-through
  (let [opts (captured-compile compile-step)]
    (is (= ["src"] (:src-dirs opts)))
    (is (= ['hive-thing.core] (:ns-compile opts)))
    (is (= "target/aot-classes" (:class-dir opts)))
    (is (= ["-Xmx2g"] (:java-opts opts)))
    (is (= ::basis (:basis opts)))))

(deftest elision-is-omitted-rather-than-emptied-when-disabled
  (testing "an explicit [] must not become {:elide-meta []}"
    (let [opts (captured-compile (assoc compile-step :step/elide-meta []))]
      (is (not (contains? opts :compile-opts))))))

(deftest java-opts-are-omitted-when-absent
  (let [opts (captured-compile (assoc compile-step :step/java-opts []))]
    (is (not (contains? opts :java-opts)))))

;; ── Version resolution ────────────────────────────────────────────────────

(deftest the-version-file-wins-when-present
  (with-redefs [io'/read-text (constantly "1.2.3\n")]
    (is (= "1.2.3" (tools/resolve-version {})))))

(deftest a-blank-version-file-falls-back-to-the-commit-count
  (testing "repos without a VERSION file still get a monotonic coordinate"
    (with-redefs [io'/read-text (constantly "  \n")
                  b/git-count-revs (constantly "417")]
      (is (= "0.7.417" (tools/resolve-version {:minor 7})))
      (is (= "0.0.417" (tools/resolve-version {}))))))

;; ── The pom that gets published ───────────────────────────────────────────

(def ^:private leaky-basis
  "A basis shaped like the fleet's: the user-level deps.edn contributes a
   private registry, which tools.build writes into every pom it generates."
  {:libs {}
   :mvn/repos {"hive-gitea" {:url "https://gitea.example.com/api/packages/hive-agi/maven"}}})

(defn- written-pom
  "The pom text the :step/write-pom handler leaves on disk for `project`."
  [project]
  (let [dir (str (java.nio.file.Files/createTempDirectory
                  "hive-build-pom" (into-array java.nio.file.attribute.FileAttribute [])))
        project (assoc project :project/class-dir dir)]
    (with-redefs [tools/pom-basis (fn [_] leaky-basis)
                  b/git-process (fn [_] "deadbeef")]
      ((get tools/handlers :step/write-pom) (tools/context project) nil))
    (slurp (b/pom-path {:lib 'io.github.hive-agi/hive-thing :class-dir dir}))))

(def ^:private thing-project
  (project/project {:lib 'io.github.hive-agi/hive-thing
                    :license {:name "MIT" :url "https://opensource.org/licenses/MIT"}
                    :scm-url "https://github.com/hive-agi/hive-thing"
                    :src-dirs ["src" "resources"]
                    :publish :gitea}
                   "1.2.3"))

(deftest the-published-pom-names-no-repository
  (testing "a <repositories> block discloses the private registry to every
            customer, and lets a resolver holding upstream credentials fetch
            the pristine jar around the store that mints and attributes it"
    (let [pom (written-pom thing-project)]
      (is (not (clojure.string/includes? pom "<repositories")))
      (is (not (clojure.string/includes? pom "gitea.example.com")))
      (testing "and the pom is otherwise intact"
        (is (clojure.string/includes? pom "<artifactId>hive-thing</artifactId>"))
        (is (clojure.string/includes? pom "<version>1.2.3</version>"))
        (is (clojure.string/includes? pom "<tag>deadbeef</tag>"))
        (is (clojure.string/includes? pom "<name>MIT</name>"))))))

(defn- published-request
  "Drive :step/publish for a gitea target with `env`, capturing what deps-deploy
   would have been handed."
  [env deps-edn servers]
  (let [captured (atom nil)
        project (project/project {:lib 'g/a :publish :gitea :src-dirs ["src"]} "1.2.3")]
    (with-redefs [io'/getenv env
                  io'/read-text (fn [p] (when (= "deps.edn" p) deps-edn))
                  maven-settings/deps-repo-by-id (fn [id] {id (get servers id)})]
      ((get tools/handlers :step/publish)
       (assoc (tools/context project) :ctx/deploy-fn #(reset! captured %))
       {:step/kind :step/publish :step/target-id :gitea :step/installer :remote}))
    @captured))

(deftest a-credential-free-deploy-is-resolved-from-deps-edn-and-settings-xml
  (testing "deps-deploy documents the repository-id form and cannot execute it,
            so the id is resolved here into the map it stands for"
    (let [request (published-request
                   {}
                   "{:mvn/repos {\"hive-gitea\" {:url \"https://g.test/maven\"}}}"
                   {"hive-gitea" {:username "bot" :password "tok"}})]
      (is (= {"hive-gitea" {:url "https://g.test/maven"
                            :username "bot"
                            :password "tok"}}
             (:repository request))))))

(deftest a-deploy-says-WHICH-half-of-the-repository-is-missing
  (testing "no :mvn/repos entry"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #":mvn/repos"
         (published-request {} "{}" {"hive-gitea" {:username "bot" :password "tok"}}))))
  (testing "no settings.xml server"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"settings.xml"
         (published-request {} "{:mvn/repos {\"hive-gitea\" {:url \"https://g.test/maven\"}}}" {})))))

(deftest env-credentials-bypass-the-settings-file-entirely
  (testing "CI has no settings.xml, and must not be made to read one"
    (let [request (published-request
                   {"MAVEN_URL" "https://ci.test/maven"
                    "MAVEN_USERNAME" "ci" "MAVEN_TOKEN" "t"}
                   nil
                   {})]
      (is (= {"hive-gitea" {:url "https://ci.test/maven"
                            :username "ci" :password "t"}}
             (:repository request))))))
