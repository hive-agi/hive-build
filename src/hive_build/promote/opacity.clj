(ns hive-build.promote.opacity
  "Does the built artifact still carry the vendor's words? Pure: the strings the
   sources declare secret go in, the strings the artifact carries go in, and the
   ones present in both come out.

   An AOT jar is audited today for what it LINKS against (`promote/classes`).
   Nothing audited what it SAYS. `:elide-meta` removes `def` metadata and the
   staging pass removes the ns docstring, but neither is observed after the
   fact, so an elision that silently stops running publishes a readable jar and
   reports success.

   The check is set intersection rather than a heuristic on purpose. A detector
   that scored strings for prose would have to guess whether
   \"Resolve user-facing cartography intent into stable indexed-world data.\" is
   a leaked docstring or a `:summary` the program prints, and it is the second
   one. Only the source knows which strings were meant to be private, so the
   source is what supplies them."
  (:require [clojure.string :as str]
            [hive-build.promote.elide :as elide]
            [malli.core :as m]))

(def ^:private preview-length
  "Characters of a leaked string a finding repeats.

   A finding names the leak; it does not reprint the secret into a build log
   that is itself less protected than the artifact."
  56)

(defn preview
  "`text` shortened to a single line no longer than `preview-length`."
  [text]
  (let [flat (-> text (str/replace #"\s+" " ") str/trim)]
    (if (<= (count flat) preview-length)
      flat
      (str (subs flat 0 preview-length) "..."))))

(def ^:private min-secret-length
  "Shortest docstring worth auditing.

   A one-word docstring collides with member names, descriptors and the
   program's own literals, and a finding nobody can act on trains an operator
   to ignore the report."
  24)

(defn source-secrets
  "The strings in `source` that AOT must not carry into the artifact.

   Two classes, both proof against `:elide-meta`, which reaches `def` metadata
   only:

     ns docstring          part of the constant map the `ns` macro emits
     defprotocol docstring part of the `:sigs` map, the protocol var's VALUE

   Docstrings shorter than `min-secret-length` are not audited."
  [source]
  (let [span->text (fn [[start end]] (subs source start end))
        ns-doc (some-> (elide/ns-docstring-span source) span->text)
        proto  (map span->text (elide/protocol-docstring-spans source))]
    (into #{}
          (comp (remove nil?)
                ;; The spans include their delimiting quotes; the artifact
                ;; carries the string's characters, not its literal syntax.
                (map #(subs % 1 (dec (count %))))
                (remove #(< (count %) min-secret-length)))
          (cons ns-doc proto))))

(defn- finding
  [kind text where]
  {:finding/kind kind :finding/text (preview text) :finding/where where})

(defn leaked
  "One finding per `secret` an artifact entry still carries.

   `constants-by-entry` maps an artifact entry name to the set of strings that
   entry holds, which for a class file is its constant pool. A secret is
   reported once per entry that carries it, because that is where the operator
   has to go to remove it."
  [secrets constants-by-entry]
  (let [secret? (set secrets)]
    (into []
          (mapcat (fn [[entry constants]]
                    (into []
                          (comp (filter secret?)
                                (map #(finding :finding/docstring % entry)))
                          (sort constants))))
          (sort-by key constants-by-entry))))

(def source-suffixes
  "Entry suffixes that make an artifact readable outright."
  #{".clj" ".cljc" ".cljs"})

(defn source-entries
  "One finding per Clojure source file `entries` carries.

   `allowed` are entry-name prefixes whose sources are published on purpose:
   clj-kondo hook exports are the fleet's case, since a linter cannot read a
   compiled hook."
  [entries {:keys [allowed] :or {allowed []}}]
  (into []
        (comp (filter (fn [e] (some #(str/ends-with? e %) source-suffixes)))
              (remove (fn [e] (some #(str/starts-with? e %) allowed)))
              (map (fn [e] (finding :finding/source-entry e e))))
        (sort entries)))

(defn audit
  "The opacity verdict for one artifact.

     :secrets            strings the sources declared private
     :constants-by-entry entry name -> strings that entry carries
     :entries            every entry name in the artifact
     :allowed-source     entry prefixes whose sources ship on purpose
     :unreadable         entries whose bytes could not be parsed

   `:opacity/leaking` states that a listed string is present in both the source
   and the artifact. It does NOT state that the artifact is safe when clean:
   this reads the text an artifact carries, and says nothing about the call
   graph, the numeric constants, or the names of anything. `:opacity/unread`
   is carried for the same reason: a verdict has to say what it did not look
   at."
  [{:keys [secrets constants-by-entry entries allowed-source unreadable]}]
  (let [findings (into (leaked secrets constants-by-entry)
                       (source-entries entries {:allowed allowed-source}))]
    {:opacity/findings findings
     :opacity/verdict (if (seq findings) :opacity/leaking :opacity/clean)
     :opacity/secrets-audited (count secrets)
     :opacity/entries-read (count constants-by-entry)
     :opacity/unread (vec unreadable)}))

(defn report
  "A build-facing description of a leaking `audit`, or nil when it is clean."
  [{:opacity/keys [findings verdict secrets-audited unread]}]
  (when (= :opacity/leaking verdict)
    (let [line (fn [{:finding/keys [kind text where]}]
                 (case kind
                   :finding/source-entry (str "  - source ships: " where)
                   (str "  - " where "\n      carries: " text)))]
      (str "Artifact is not opaque: " (count findings) " leak(s) of "
           secrets-audited " audited string(s).\n"
           (str/join "\n" (map line findings))
           (when (seq unread)
             (str "\n" (count unread) " entry(s) could not be read and were"
                  " not audited: " (str/join ", " (take 5 unread))))
           "\nA docstring here survived :elide-meta because it is form DATA,"
           " not def metadata. Strip it on the staged copy"
           " (:aot/elide-meta must contain :doc), or declare the entry's"
           " sources publishable."))))

(m/=> preview [:=> [:cat :string] :string])
(m/=> source-secrets [:=> [:cat :string] [:set :string]])
(m/=> leaked [:=> [:cat [:sequential :string] [:map-of :string [:set :string]]]
              [:vector :map]])
(m/=> source-entries [:=> [:cat [:sequential :string] :map] [:vector :map]])
(m/=> audit [:=> [:cat :map] :map])
(m/=> report [:=> [:cat :map] [:maybe :string]])
