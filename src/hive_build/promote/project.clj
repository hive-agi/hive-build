(ns hive-build.promote.project
  "The Project value object: raw version.edn data plus a resolved version, with
   every default applied. Pure.

   Nothing downstream reads version.edn again, so a default lives here exactly
   once."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [hive-build.promote.naming :as naming]
            [hive-build.schema :as s]))

(def default-elide-meta
  "Metadata keys stripped from every AOT class file unless version.edn
   overrides :aot/elide-meta. Docstrings, arglists and source coordinates ship
   as string constants in bytecode.

   :column is deliberately absent. Eliding it alongside these keys makes the
   compiler fail on some dependency graphs, and a column number on its own
   discloses nothing that :file and :line do not."
  [:doc :file :line :added :arglists])

(def source-extensions
  #{".clj" ".cljc" ".cljs"})

(defn source-file?
  "True when `path` names a compilable source that is not a clj-kondo export."
  [path]
  (boolean (and (some #(str/ends-with? path %) source-extensions)
                (not (str/includes? path "clj-kondo.exports")))))

(defn source-root?
  "True when `paths` — the files beneath one directory — contains a source."
  [paths]
  (boolean (some source-file? paths)))

(defn classify-roots
  "`src-dirs` split into roots holding sources and roots holding only
   resources. `files-by-dir` maps each dir to the paths beneath it.

   Contract: the two results partition `src-dirs` — every root is classified,
   none is classified twice."
  [src-dirs files-by-dir]
  (let [sources (filterv #(source-root? (get files-by-dir % [])) src-dirs)]
    {:facts/source-roots sources
     :facts/resource-roots (vec (remove (set sources) src-dirs))}))

(defn- license-of
  [cfg]
  (let [{:keys [name url]} (:license cfg)]
    (when (and (string? name) (not (str/blank? name)))
      {:license/name name :license/url (or url "")})))

(defn project
  "A Project from raw version.edn `cfg` and the resolved `version` string.

   Throws when :lib is not a qualified symbol: nothing downstream can name an
   artifact without it, and a build that guesses is worse than one that stops."
  [cfg version]
  (let [lib (:lib cfg)]
    (when-not (qualified-symbol? lib)
      (throw (ex-info "version.edn :lib must be a qualified symbol group/artifact"
                      {:lib lib})))
    (let [coord      (naming/coordinate lib version)
          target-dir (:target-dir cfg "target")]
      {:project/coordinate       coord
       :project/src-dirs         (vec (:src-dirs cfg ["src"]))
       :project/target-dir       target-dir
       :project/class-dir        (str target-dir "/classes")
       :project/scratch-dir      (str target-dir "/aot-classes")
       :project/jar-file         (naming/jar-file target-dir coord)
       :project/target-id        (:publish cfg :none)
       :project/license          (license-of cfg)
       :project/scm-url          (:scm-url cfg)
       :project/elide-meta       (vec (:aot/elide-meta cfg default-elide-meta))
       :project/pom-exclude-deps (set (:pom-exclude-deps cfg []))
       :project/aot-java-opts    (vec (:aot/java-opts cfg []))})))

(m/=> source-file? [:=> [:cat :string] :boolean])
(m/=> source-root? [:=> [:cat [:sequential :string]] :boolean])
(m/=> classify-roots
      [:=> [:cat [:vector [:string {:min 1}]] [:map-of :string [:sequential :string]]]
       [:map {:closed true}
        [:facts/source-roots [:vector [:string {:min 1}]]]
        [:facts/resource-roots [:vector [:string {:min 1}]]]]])
(m/=> project [:=> [:cat :map s/VersionString] s/Project])
