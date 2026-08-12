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

(defn class-names
  "Internal names of every class the class file in `bytes` references.

   Reads the constant pool only: CONSTANT_Class entries and the CONSTANT_Utf8
   entries they point at. Array descriptors ([Lfoo/Bar;) are unwrapped to the
   element class."
  [^bytes bytes]
  (let [count' (u2 bytes 8)]
    (loop [i 10, n 1, utf8 {}, refs []]
      (if (>= n count')
        (into #{}
              (comp (keep utf8)
                    (map #(str/replace % #"^\[+L?" ""))
                    (map #(str/replace % #";$" ""))
                    (remove str/blank?))
              refs)
        (let [tag (u1 bytes i)
              [width wide?] (entry-width tag bytes (inc i))
              body (inc i)]
          (recur (+ body width)
                 (+ n (if wide? 2 1))
                 (if (= 1 tag)
                   (assoc utf8 n (String. bytes (+ body 2) (u2 bytes body) "UTF-8"))
                   utf8)
                 (if (= 7 tag) (conj refs (u2 bytes body)) refs)))))))

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

(m/=> class-names [:=> [:cat :any] [:set :string]])
(m/=> foreign-refs [:=> [:cat [:sequential :string] :map] [:vector :string]])
(m/=> report [:=> [:cat [:sequential :string]] [:maybe :string]])
