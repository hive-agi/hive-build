(ns hive-build.api
  "The canonical hive release tasks, delivered as a library.

   Point a repository's :build alias here and it inherits the whole release
   path — no build.clj to copy, and no build.clj to drift:

     :build {:deps {io.github.hive-agi/hive-build {:mvn/version \"0.1.0\"}}
             :ns-default hive-build.api}

   Coordinates come from ./version.edn, the version from ./VERSION:

     {:lib      io.github.hive-agi/hive-thing
      :minor    1
      :license  {:name \"MIT\" :url \"https://opensource.org/licenses/MIT\"}
      :scm-url  \"https://github.com/hive-agi/hive-thing\"
      :src-dirs [\"src\"]
      :publish  :clojars             ; :clojars | :gitea | :gitea-source | :none
      :aot/java-opts []              ; optional, AOT compile only
      :aot/elide-meta []             ; optional, [] disables metadata elision
      :pom-exclude-deps []}          ; optional, dropped from the published pom

   An untracked ./local.deps.edn may supply a `:provided` alias (host sources
   that must be on the AOT compile classpath but must NOT enter the pom) and an
   `:aot/preload` namespace vector compiled ahead of this lib's own namespaces.

   `:publish` is the ONLY thing that differs between packages — the task names
   are identical everywhere, so one CI workflow drives the whole fleet:

     :clojars       public source jar   -> repo.clojars.org
     :gitea         AOT no-source jar   -> private Gitea Maven registry
     :gitea-source  source jar          -> private Gitea Maven registry
     :none          builds, never ships

   Tasks (invoke with `clojure -T:build <task>`):
     clean           delete target/
     jar             source jar + pom
     jar-aot         AOT no-source jar (own .class + resources only)
     install         build + install to ~/.m2 (offline)
     kondo           sync dependency-exported lint configs, then lint
     bump            rewrite ./VERSION (:level :patch|:minor|:major)
     verify-license  report LICENSE / version.edn / SPDX agreement (warns)
     freeze-check    refuse a release that breaks ./freeze-policy.edn (fails)
     deploy          build + publish per :publish (no-op when :none)

   Release flow (what CI runs on a push to main that touches src/deps):
     clojure -T:build bump :level :patch
     clojure -T:build deploy"
  (:require [clojure.string :as str]
            [hive-build.boundary.freeze :as gitf]
            [hive-build.boundary.run :as run]
            [hive-build.boundary.tools :as tools]
            [hive-build.collect.io :as io']
            [hive-build.pipeline.plan :as plan]
            [hive-build.promote.license :as license]
            [hive-build.promote.api-surface :as surface]
            [hive-build.promote.freeze :as freeze]
            [hive-build.promote.lint :as lint]
            [hive-build.promote.naming :as naming]
            [hive-build.promote.version :as version]))

(defn- execute!
  [task {:keys [deploy-fn probe-registry?]}]
  (let [project (tools/read-project)
        overlay (when (plan/compiles? task project) (tools/overlay))
        facts (tools/read-facts project {:probe-registry? (boolean probe-registry?)
                                         :overlay overlay})]
    (run/run! tools/handlers (tools/context project overlay deploy-fn)
              (plan/plan task project facts))))

;; ── Build ──────────────────────────────────────────────────────────────────

(defn clean
  "Delete target/."
  [_]
  (execute! :task/clean {}))

(defn jar
  "Build the source jar (pom + copied sources) under target/."
  [_]
  (execute! :task/jar {}))

(defn jar-aot
  "Build the AOT no-source jar: this lib's own .class files + resources only."
  [_]
  (execute! :task/jar-aot {}))

(defn install
  "Build + install to the local ~/.m2 repository (offline)."
  [_]
  (execute! :task/install {}))

;; ── Version ────────────────────────────────────────────────────────────────

(defn bump
  "Rewrite ./VERSION to the next semantic version and print it.

   :level :patch (default) | :minor | :major
   VERSION is the single source of truth for both the git tag (v{VERSION}) and
   the Maven coordinate. Does not commit, tag, or deploy."
  [{:keys [level] :or {level :patch}}]
  (let [current (or (some-> (io'/read-text "VERSION") str/trim not-empty)
                    (throw (ex-info "No ./VERSION file to bump"
                                    {:cwd (System/getProperty "user.dir")})))
        next-v (version/next-version current level)]
    (io'/write-text! "VERSION" (str next-v "\n"))
    (println (format "VERSION %s -> %s (%s)" current next-v (name level)))
    next-v))

;; ── Licence ────────────────────────────────────────────────────────────────

(defn verify-license
  "Report whether ./LICENSE, version.edn :license and the src SPDX headers
   agree. Advisory: prints and returns the report, never fails the build."
  [_]
  (let [project (tools/read-project)
        report (license/report (tools/read-license-facts project))
        label (naming/coordinate-label (:project/coordinate project))]
    (if (:report/ok? report)
      (println "License OK:" (get-in project [:project/license :license/name]))
      (do (println "WARNING: license inconsistency in" label)
          (doseq [p (:report/problems report)] (println "  -" p))
          (println "  A published pom can never be retracted.")))
    report))

(defn freeze-check
  "Refuse a release that breaks the repository's freeze policy.

   Reads ./freeze-policy.edn (absent = nothing enforced), compares the public
   API surface on disk against the surface at the last v* tag, and checks the
   age of that tag. Prints the verdict; THROWS on a violation so the release
   workflow stops before `bump` mints a version that can never be retracted.

   Escape hatches are per-commit and must be written down: the cadence marker
   and the break marker named by the policy, in the HEAD commit message.

   Returns the verdict map on success."
  [_]
  (let [project  (tools/read-project)
        src-dirs (or (:project/src-dirs project) ["src"])
        policy   (or (io'/read-edn "freeze-policy.edn") freeze/default-policy)
        tag      (gitf/last-release-tag)
        old      (when tag
                   (->> (gitf/files-at tag src-dirs)
                        (keep #(gitf/text-at tag %))
                        (map gitf/read-forms)
                        (map surface/surface)
                        surface/merge-surfaces))
        new      (->> (gitf/source-files src-dirs)
                      (keep gitf/read-text)
                      (map gitf/read-forms)
                      (map surface/surface)
                      surface/merge-surfaces)
        d        (surface/diff (or old {}) new)
        verdict  (freeze/evaluate
                  policy
                  {:diff d
                   :breaking? (and (some? tag) (surface/breaking? d))
                   :descriptions (surface/describe d)
                   :days-since-last (gitf/tag-age-days tag)
                   :commit-message (gitf/head-commit-message)})]
    (println (str "freeze-check: " (count new) " public names, baseline "
                  (or tag "none (first release)")
                  (when tag (str ", +" (count (:added d)) " added"))))
    (doseq [line (freeze/report-lines verdict)] (println line))
    (when-not (:ok? verdict)
      (throw (ex-info "freeze-check refused this release" verdict)))
    verdict))

;; ── Lint ───────────────────────────────────────────────────────────────────

(defn kondo
  "Sync clj-kondo configs exported by dependencies, then lint.

   Any deps.edn or bb.edn dependency shipping
   resources/clj-kondo.exports/<group>/<artifact>/ has its config + hooks
   copied into ./.clj-kondo/imports/, which clj-kondo loads automatically.
   Macro awareness therefore arrives with the dependency instead of being
   re-authored per repo.

   :aliases    deps aliases whose classpath is scanned  (default [:test])
   :paths      lint targets                             (default src + test)
   :fail-level :error (default) | :warning | nil to report only"
  [{:keys [aliases paths fail-level]
    :or {aliases [:test] fail-level :error}}]
  (if-not (tools/kondo-available?)
    (println "Skip: clj-kondo not on PATH — install it to sync lint configs.")
    (let [project (tools/read-project)
          cp (lint/classpath [(tools/deps-classpath aliases) (tools/bb-classpath)])
          targets (or (seq paths)
                      (filterv io'/exists?
                               (lint/lint-candidates (:project/src-dirs project))))]
      (tools/run-process (lint/sync-command cp))
      (let [{:keys [exit]} (tools/run-process (lint/lint-command (vec targets) fail-level))]
        (when (and fail-level (pos? exit))
          (throw (ex-info "clj-kondo reported findings at or above :fail-level"
                          {:fail-level fail-level :exit exit})))
        {:exit exit}))))

;; ── Publish ────────────────────────────────────────────────────────────────

(defn deploy
  "Build + publish according to version.edn :publish.

   A coordinate already present in the registry is a no-op, not an error:
   both registries are immutable, so releasing again means bumping VERSION."
  [opts]
  (verify-license opts)
  (execute! :task/deploy (assoc opts :probe-registry? true)))
