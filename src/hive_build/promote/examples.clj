(ns hive-build.promote.examples
  "README code blocks as executable claims. Pure.

   A fenced ```clojure block in a markdown file claims that its code runs
   against this library, and a `;; => value` line after a form claims what
   that form answers. `blocks` finds the claims, `examples` reads a block into
   forms paired with their claimed values, and `script` renders one Clojure
   program that evaluates them in order, names each before it runs, and exits
   non-zero when a form throws or answers something else. The program runs in
   a cold JVM at the boundary (`hive-build.api/readme-examples`), never in the
   session that wrote the README.

   Add `no-run` to a fence's info string (```clojure no-run) to keep a block
   out of the run. Rationale: hive memory 20260907084849-421ab665."
  (:require [clojure.string :as str]
            [malli.core :as m])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io StringReader]))

(def runnable-langs
  "Fence languages whose blocks are Clojure programs."
  #{"clojure" "clj"})

(def skip-marker
  "Word in a fence's info string that keeps the block out of the run."
  "no-run")

(defn- fence-line? [line] (str/starts-with? (str/triml line) "```"))

(defn- fence-free? [source]
  (not-any? fence-line? (str/split-lines (or source ""))))

(defn- cr-free? [s] (not (str/includes? (or s "") "\r")))

(def Source
  "A block body: lines that never close the fence, split on newlines only."
  [:and :string
   [:fn {:error/message "a block body cannot contain a fence line"} fence-free?]
   [:fn {:error/message "a block body has no carriage returns"} cr-free?]])

(def Info
  "A fence's info string: the rest of the opening line, one line by construction."
  [:and :string [:fn {:error/message "an info string is one line"} #(not (str/includes? % "\n"))]])

(def Block
  [:map {:closed true}
   [:block/index [:int {:min 0}]]
   [:block/line [:int {:min 1}]]
   [:block/lang :string]
   [:block/info Info]
   [:block/source Source]
   [:block/run? :boolean]])

(def Example
  [:map {:closed true}
   [:example/block [:int {:min 0}]]
   [:example/line [:int {:min 1}]]
   [:example/source :string]
   [:example/expected [:maybe :string]]])

(def Summary
  [:map {:closed true}
   [:examples/blocks [:int {:min 0}]]
   [:examples/runnable [:int {:min 0}]]
   [:examples/skipped [:int {:min 0}]]
   [:examples/claims [:int {:min 0}]]])

(defn- info-of [line] (str/trim (subs (str/triml line) 3)))

(defn lang-of
  "The language a fence's info string names; \"\" when it names none."
  [info]
  (or (first (str/split (str/trim (or info "")) #"\s+")) ""))

(defn runnable?
  "True when a block with this language and info string is part of the run."
  [lang info]
  (and (contains? runnable-langs lang)
       (not (contains? (set (str/split (or info "") #"\s+")) skip-marker))))

(defn blocks
  "Every fenced code block of `markdown`, in document order, with the 1-based
   line of its opening fence. A block still open at the end of the text is
   not a block."
  [markdown]
  (loop [lines (seq (map-indexed vector (str/split-lines (or markdown ""))))
         open  nil
         acc   []]
    (if-let [[i raw] (first lines)]
      (let [line (str/replace raw "\r" "")]
        (cond
          (and open (fence-line? line))
          (let [{:keys [at info buf]} open
                lang (lang-of info)]
            (recur (next lines) nil
                   (conj acc {:block/index  (count acc)
                              :block/line   at
                              :block/lang   lang
                              :block/info   info
                              :block/source (str/join "\n" buf)
                              :block/run?   (runnable? lang info)})))

          open
          (recur (next lines) (update open :buf conj line) acc)

          (fence-line? line)
          (recur (next lines) {:at (inc i) :info (info-of line) :buf []} acc)

          :else
          (recur (next lines) nil acc)))
      acc)))

(defn markdown
  "A markdown document holding exactly `blocks`, each under a line of prose:
   the inverse of `blocks` for their info strings and sources."
  [blocks]
  (str/join "\n" (mapcat (fn [{:block/keys [info source]}]
                           ["prose" (str "```" info) source "```"])
                         blocks)))

(def ^:private claim-re #"^\s*;;\s*=>\s*(.*\S)\s*$")

(defn- claim-after
  "The `;; => value` line that follows line `end` (1-based) after any blank
   lines, or nil when the next thing is not a claim."
  [lines end]
  (some->> (drop end lines)
           (drop-while str/blank?)
           first
           (re-matches claim-re)
           second))

(defn- form-start
  "[line col] of the first character at or after [line col] that is neither
   whitespace nor inside a line comment; nil when no such character remains.
   Lines are 1-based, columns 0-based."
  [lines line col]
  (loop [l line c col]
    (when (<= l (count lines))
      (let [text (nth lines (dec l))
            ch   (when (< c (count text)) (nth text c))]
        (cond
          (nil? ch)                                          (recur (inc l) 0)
          (= \; ch)                                          (recur (inc l) 0)
          (or (Character/isWhitespace ^char ch) (= \, ch))   (recur l (inc c))
          :else                                              [l c])))))

(defn- spans
  "[[start-line start-col end-line end-col] ...] of every top-level form in
   `source`: 1-based lines, 0-based columns, end exclusive. Throws when the
   reader cannot take the source apart.

   The reader answers where a form ENDS; where it starts is found by walking
   forward from the previous end over whitespace and line comments, which is
   exact for every form kind where the reader's own metadata only covers
   lists. A token closed by a newline is reported on the line after it at
   column 0, and the end of input is reported as a line past the last one;
   both mean the form ran to the end of what came before. A form is never
   empty, so an end that does not follow its start also means the form runs
   to the end of the source."
  [source]
  (let [lines      (vec (str/split-lines source))
        rdr        (LineNumberingPushbackReader. (StringReader. source))
        eof        (Object.)
        last-line  (count lines)
        source-end [last-line (count (nth lines (dec last-line) ""))]]
    (binding [*read-eval* false]
      (loop [acc [] from [1 0]]
        (let [form (read {:eof eof :read-cond :allow} rdr)]
          (if (identical? eof form)
            acc
            (let [l       (.getLineNumber rdr)
                  c       (.getColumnNumber rdr)
                  [sl sc] (or (apply form-start lines from) from)
                  [el ec] (cond
                            (> l last-line) source-end
                            (zero? c)       [(dec l) (count (nth lines (- l 2) ""))]
                            :else           [l (dec c)])
                  end     (if (or (< el sl) (and (= el sl) (<= ec sc))) source-end [el ec])]
              (recur (conj acc [sl sc (first end) (second end)]) end))))))))

(defn- slice
  "The verbatim text of `lines` between a span's start and end."
  [lines [sl sc el ec]]
  (let [el   (min el (count lines))
        last (nth lines (dec el))]
    (if (= sl el)
      (subs last sc (min ec (count last)))
      (str/join "\n" (concat [(subs (nth lines (dec sl)) sc)]
                             (subvec lines sl (dec el))
                             [(subs last 0 (min ec (count last)))])))))

(defn examples
  "The forms of a runnable block, each verbatim and with the value the README
   claims for it; nil for a block that is not run. A claim belongs to the
   LAST form on its line. A block the reader cannot take apart is one
   example with no claim, so the cold JVM reports the real error."
  [{:block/keys [index source run?]}]
  (when run?
    (let [lines (vec (str/split-lines source))
          spans (try (spans source) (catch Throwable _ nil))]
      (if (empty? spans)
        [{:example/block index :example/line 1 :example/source source :example/expected nil}]
        (mapv (fn [[sl _ el _ :as span] next-span]
                {:example/block    index
                 :example/line     sl
                 :example/source   (slice lines span)
                 :example/expected (when-not (and next-span (= el (first next-span)))
                                     (claim-after lines el))})
              spans
              (concat (rest spans) [nil]))))))

(def ^:private harness-forms
  '[(def readme-examples--failures (atom 0))
    (defn readme-examples--at [block line]
      (println (str "readme-examples: block " block " line " line)))
    (defn readme-examples--check [block line actual expected]
      (let [claimed (try (binding [*read-eval* false] (read-string expected))
                         (catch Throwable _ ::unreadable))
            ok?     (or (= (pr-str actual) expected) (= actual claimed))]
        (when-not ok?
          (println (str "readme-examples: block " block " line " line
                        " claims " expected " but got " (pr-str actual)))
          (swap! readme-examples--failures inc))
        actual))
    (defn readme-examples--exit []
      (let [n @readme-examples--failures]
        (println (str "readme-examples: " n " refuted claim(s)"))
        (shutdown-agents)
        (System/exit (if (pos? n) 1 0))))])

(defn script
  "One Clojure program that evaluates every runnable block's forms in order,
   names each before it runs, and refutes claims as it goes. Meant for a new
   JVM: it ends the process with the verdict as its exit code."
  [blocks]
  (let [exs (mapcat examples blocks)]
    (str (str/join "\n" (map pr-str harness-forms)) "\n"
         (str/join "\n"
                   (map (fn [{:example/keys [block line source expected]}]
                          (str (format "(user/readme-examples--at %d %d)\n" block line)
                               (if expected
                                 (format "(user/readme-examples--check %d %d (do\n%s\n) %s)"
                                         block line source (pr-str expected))
                                 source)))
                        exs))
         "\n(user/readme-examples--exit)\n")))

(defn summary
  "How much a document claims: blocks found, run, kept out, and claimed values."
  [blocks]
  (let [run (filter :block/run? blocks)]
    {:examples/blocks   (count blocks)
     :examples/runnable (count run)
     :examples/skipped  (- (count blocks) (count run))
     :examples/claims   (count (filter :example/expected (mapcat examples run)))}))

(m/=> lang-of   [:=> [:cat [:maybe :string]] :string])
(m/=> runnable? [:=> [:cat :string [:maybe :string]] :boolean])
(m/=> blocks    [:=> [:cat [:maybe :string]] [:sequential Block]])
(m/=> markdown  [:=> [:cat [:sequential Block]] :string])
(m/=> examples  [:=> [:cat Block] [:maybe [:sequential Example]]])
(m/=> script    [:=> [:cat [:sequential Block]] :string])
(m/=> summary   [:=> [:cat [:sequential Block]] Summary])
