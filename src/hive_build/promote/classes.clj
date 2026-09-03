(ns hive-build.promote.classes
  "Compiled classes as values: which classes a .class file links against, and
   which of those links the jar does not satisfy. Pure.

   An AOT jar that hardcodes `implements hive_addon.protocol.IAddon` builds and
   publishes in silence, then fails to mount because the host loaded that
   protocol from source under a different class identity. The link is visible
   in the constant pool long before anyone tries to mount it."
  (:require [clojure.string :as str]
            [malli.core :as m]))

(def default-foreign-prefixes
  "Class-name prefixes a hive jar is audited for."
  ["hive_"])

(defn internal-name
  "Any spelling of a class name as the internal (slash) form a constant pool
   carries: hive-addon.protocol.IAddon -> hive_addon/protocol/IAddon."
  [x]
  (-> (str x) (str/replace "-" "_") (str/replace "." "/")))

(defn- u1 [^bytes bs i] (bit-and (aget bs i) 0xff))
(defn- u2 [^bytes bs i] (bit-or (bit-shift-left (u1 bs i) 8) (u1 bs (inc i))))

(defn- entry-width
  "Bytes the constant-pool entry with `tag` occupies after its tag byte, and
   whether it consumes a second pool slot (long and double do)."
  [tag ^bytes bs i]
  (case tag
    1  [(+ 2 (u2 bs i)) false]
    (3 4 9 10 11 12 17 18) [4 false]
    (5 6) [8 true]
    (7 8 16 19 20) [2 false]
    15 [3 false]
    (throw (ex-info "unknown constant-pool tag" {:tag tag :offset i}))))

(defn constant-pool
  "The class file's constant pool, read once: `{:utf8 {slot string}
   :class-refs [slot]}`.

   One walker, because every question this namespace answers is a projection of
   the same pass. `:utf8` carries EVERY CONSTANT_Utf8 entry, not only the ones a
   CONSTANT_Class points at, so a caller auditing what text the class ships sees
   the same pool the linker does."
  [^bytes bytes]
  (let [count' (u2 bytes 8)]
    (loop [i 10, n 1, utf8 {}, refs []]
      (if (>= n count')
        {:utf8 utf8 :class-refs refs}
        (let [tag (u1 bytes i)
              [width wide?] (entry-width tag bytes (inc i))
              body (inc i)]
          (recur (+ body width)
                 (+ n (if wide? 2 1))
                 (if (= 1 tag)
                   (assoc utf8 n (String. bytes (+ body 2) (u2 bytes body) "UTF-8"))
                   utf8)
                 (if (= 7 tag) (conj refs (u2 bytes body)) refs)))))))

(defn class-names
  "Internal names of every class the class file in `bytes` references.

   Reads the constant pool only: CONSTANT_Class entries and the CONSTANT_Utf8
   entries they point at. Array descriptors ([Lfoo/Bar;) are unwrapped to the
   element class."
  [^bytes bytes]
  (let [{:keys [utf8 class-refs]} (constant-pool bytes)]
    (into #{}
          (comp (keep utf8)
                (map #(str/replace % #"^\[+L?" ""))
                (map #(str/replace % #";$" ""))
                (remove str/blank?))
          class-refs)))

(defn utf8-constants
  "Every CONSTANT_Utf8 string the class file in `bytes` carries, deduplicated.

   This is the text a `strings` pass over the artifact would recover, minus the
   guesswork: descriptors, member names, and any string literal the compiler
   interned all sit in the same pool."
  [^bytes bytes]
  (into #{} (vals (:utf8 (constant-pool bytes)))))

(defn foreign-refs
  "The audited class names in `names` that this jar neither owns nor ships.

   :prefixes — classpath prefixes of the namespaces the jar packages
   :shipped  — internal names of classes present in the jar
   :allowed  — internal names declared acceptable in version.edn
   :audited  — name prefixes to audit (default: hive_)"
  [names {:keys [prefixes shipped allowed audited]
          :or {audited default-foreign-prefixes}}]
  (let [shipped? (set shipped)
        allowed? (set allowed)
        own?     (fn [n] (some #(str/starts-with? n %) prefixes))]
    (into []
          (comp (filter (fn [n] (some #(str/starts-with? n %) audited)))
                (remove own?)
                (remove shipped?)
                (remove allowed?)
                (distinct))
          (sort names))))

(defn report
  "A build-facing description of `offenders`, or nil when there are none."
  [offenders]
  (when (seq offenders)
    (str "AOT jar links against " (count offenders)
         " foreign class(es) it does not ship:\n"
         (str/join "\n" (map #(str "  - " (str/replace % "/" ".")) offenders))
         "\nEither package them (:aot/package-protocols) or declare them"
         " (:aot/allow-foreign-classes) in version.edn.")))

(defn unpackaged
  "The declared protocol class paths in `declared` that `copied` does not
   contain.

   `:aot/package-protocols` names classes by hand, so a namespace that was
   required rather than compiled — or a protocol since renamed — leaves
   nothing at the declared path for the copy step to find."
  [declared copied]
  (let [copied? (set copied)]
    (into [] (comp (remove copied?) (distinct)) declared)))

(defn unpackaged-report
  "A build-facing description of the `absent` declared protocol classes, or nil
   when every declared class was produced."
  [absent]
  (when (seq absent)
    (str ":aot/package-protocols declares " (count absent)
         " protocol class(es) the AOT compile did not produce:\n"
         (str/join "\n" (map #(str "  - " %) absent))
         "\nEither the namespace was required instead of compiled, or the"
         " protocol was renamed. Fix version.edn or the compile set.")))

(m/=> constant-pool [:=> [:cat :any] [:map [:utf8 [:map-of :int :string]]
                                          [:class-refs [:vector :int]]]])
(m/=> class-names [:=> [:cat :any] [:set :string]])
(m/=> utf8-constants [:=> [:cat :any] [:set :string]])
(m/=> foreign-refs [:=> [:cat [:sequential :string] :map] [:vector :string]])
(m/=> report [:=> [:cat [:sequential :string]] [:maybe :string]])

(m/=> unpackaged [:=> [:cat [:sequential :string] [:sequential :string]] [:vector :string]])
(m/=> unpackaged-report [:=> [:cat [:sequential :string]] [:maybe :string]])
