(ns hive-build.promote.reproducible
  "What must be rewritten INSIDE a jar for two builds of identical inputs to be
   byte-identical. Pure.

   Fixing ZIP entry timestamps is not enough: build tooling also writes wall
   clock time and host details into entry content, where a timestamp rewrite
   cannot reach them. Each such source is a rule, so a newly discovered one is
   an addition rather than an edit."
  (:require [clojure.string :as str]
            [malli.core :as m]))

(defprotocol IEntryRule
  (rule-id [this] "Keyword identifying this rule.")
  (applies? [this entry-name] "True when this rule claims the named entry.")
  (rewrite [this text] "The entry's text with this source of variance removed."))

(defn- lines-of
  "`text` split on newlines, losslessly.

   clojure.string/split-lines discards trailing empty lines, so rejoining its
   result is not the identity: a rewritten manifest would lose the newline the
   JAR specification requires, and rewriting twice would not agree with
   rewriting once."
  [text]
  (str/split text #"\n" -1))

(defn strip-comments
  "`text` without its comment lines, leaving its line structure otherwise
   intact.

   java.util.Properties/store stamps the current time as a comment, so two
   builds seconds apart differ. Maven reads no meaning from a comment."
  [text]
  (->> (lines-of text)
       (remove #(str/starts-with? (str/triml %) "#"))
       (str/join "\n")))

(defn drop-headers
  "`text` — a MANIFEST-style header block — without the headers named in
   `names`, leaving its line structure otherwise intact."
  [names text]
  (->> (lines-of text)
       (remove (fn [line] (some #(str/starts-with? line (str % ":")) names)))
       (str/join "\n")))

(def ^:private maven-pom-properties
  (reify IEntryRule
    (rule-id [_] :maven-pom-properties)
    (applies? [_ entry-name] (str/ends-with? entry-name "pom.properties"))
    (rewrite [_ text] (strip-comments text))))

(def ^:private jar-manifest
  (reify IEntryRule
    (rule-id [_] :jar-manifest)
    (applies? [_ entry-name] (= "META-INF/MANIFEST.MF" entry-name))
    ;; Build-Jdk-Spec is the JDK that happened to run the build. Keeping it
    ;; would make an artifact reproducible on one machine and nowhere else,
    ;; which is the case that matters least.
    (rewrite [_ text] (drop-headers ["Build-Jdk-Spec"] text))))

(def default-rules
  [maven-pom-properties jar-manifest])

(defn nondeterministic?
  "True when any rule claims `entry-name`. Entries no rule claims are copied
   byte for byte and never decoded, so binary content is untouched."
  ([entry-name] (nondeterministic? default-rules entry-name))
  ([rules entry-name] (boolean (some #(applies? % entry-name) rules))))

(defn normalize-text
  "`text` of the entry named `entry-name` with every applicable rule applied."
  ([entry-name text] (normalize-text default-rules entry-name text))
  ([rules entry-name text]
   (reduce (fn [acc rule] (if (applies? rule entry-name) (rewrite rule acc) acc))
           text
           rules)))

(m/=> strip-comments [:=> [:cat :string] :string])
(m/=> drop-headers [:=> [:cat [:sequential :string] :string] :string])
(m/=> nondeterministic? [:function
                         [:=> [:cat :string] :boolean]
                         [:=> [:cat [:sequential :any] :string] :boolean]])
(m/=> normalize-text [:function
                      [:=> [:cat :string :string] :string]
                      [:=> [:cat [:sequential :any] :string :string] :string]])
