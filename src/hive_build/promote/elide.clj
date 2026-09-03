(ns hive-build.promote.elide
  "Source-level metadata elision, applied to a staged copy of the sources
   before AOT. Pure: text in, text out.

   The compiler's :elide-meta reaches `def` metadata only. An `ns` form's
   docstring is part of a constant map the ns macro emits, so it survives into
   the generated __init class verbatim."
  (:require [clojure.string :as str]
            [malli.core :as m]))

(def source-extensions
  "File suffixes whose ns form the staging pass rewrites."
  #{".clj" ".cljc" ".cljs"})

(defn clojure-source?
  "True when `path` names a file the staging pass should rewrite."
  [path]
  (boolean (some #(str/ends-with? path %) source-extensions)))

;; ── A scanner, not a reader ───────────────────────────────────────────────
;; Only the docstring's own characters are located; every other byte of the
;; file is preserved, so reader conditionals and metadata survive untouched.

(defn- delimiter?
  [^Character c]
  (or (Character/isWhitespace c)
      (contains? #{\( \) \[ \] \{ \} \" \; \, \'} c)))

(defn- skip-blanks
  "Index of the first character at or after `i` that is neither whitespace,
   a comma, nor part of a `;` line comment. `(count s)` when there is none."
  [^String s i]
  (let [n (.length s)]
    (loop [i i]
      (cond
        (>= i n) n
        (Character/isWhitespace (.charAt s i)) (recur (inc i))
        (= \, (.charAt s i)) (recur (inc i))
        (= \; (.charAt s i)) (let [nl (.indexOf s "\n" (int i))]
                               (if (neg? nl) n (recur (inc nl))))
        :else i))))

(defn- string-end
  "Index just past the string literal opening at `i`, or nil when unterminated."
  [^String s i]
  (let [n (.length s)]
    (loop [j (inc i)]
      (cond
        (>= j n) nil
        (= \\ (.charAt s j)) (recur (+ j 2))
        (= \" (.charAt s j)) (inc j)
        :else (recur (inc j))))))

(declare form-end)

(defn- delimited-end
  "Index just past the balanced form opening at `i` with `close`."
  [^String s i ^Character close]
  (let [n (.length s)
        open (.charAt s i)]
    (loop [j (inc i) depth 1]
      (cond
        (>= j n) nil
        (= \" (.charAt s j)) (if-let [e (string-end s j)] (recur e depth) nil)
        (= \; (.charAt s j)) (let [nl (.indexOf s "\n" (int j))]
                               (if (neg? nl) nil (recur (inc nl) depth)))
        (= \\ (.charAt s j)) (recur (+ j 2) depth)
        (= open (.charAt s j)) (recur (inc j) (inc depth))
        (= close (.charAt s j)) (if (= 1 depth) (inc j) (recur (inc j) (dec depth)))
        :else (recur (inc j) depth)))))

(defn- form-end
  "Index just past the single form starting at `i`, or nil when `s` ends first.
   Handles ^metadata and # dispatch prefixes by skipping to the form they
   decorate."
  [^String s i]
  (let [n (.length s)]
    (when (< i n)
      (let [c (.charAt s i)]
        (cond
          (or (= \^ c) (= \# c) (= \' c) (= \@ c) (= \~ c) (= \` c))
          (when-let [j (form-end s (skip-blanks s (inc i)))]
            j)

          (= \" c) (string-end s i)
          (= \( c) (delimited-end s i \))
          (= \[ c) (delimited-end s i \])
          (= \{ c) (delimited-end s i \})
          (= \\ c) (min n (+ i 2))
          :else (loop [j i]
                  (if (or (>= j n) (delimiter? (.charAt s j))) j (recur (inc j)))))))))

(defn ns-docstring-span
  "`[start end)` of the docstring literal in `source`'s leading ns form, or nil
   when the file does not open with an ns form that declares one.

   Refuses rather than guesses: anything it cannot parse yields nil, and the
   file is then staged unchanged."
  [^String source]
  (let [n (.length source)
        open (skip-blanks source 0)]
    (when (and (< open n) (= \( (.charAt source open)))
      (let [head (skip-blanks source (inc open))]
        (when (and (< (+ head 2) n)
                   (= "ns" (subs source head (+ head 2)))
                   (delimiter? (.charAt source (+ head 2))))
          (loop [i (skip-blanks source (+ head 2))]
            (when (< i n)
              (if (= \^ (.charAt source i))
                (when-let [e (form-end source i)]
                  (recur (skip-blanks source e)))
                ;; i is the ns name; the docstring, if any, follows it
                (when-let [name-end (form-end source i)]
                  (let [doc (skip-blanks source name-end)]
                    (when (and (< doc n) (= \" (.charAt source doc)))
                      (when-let [e (string-end source doc)]
                        [doc e]))))))))))))

(defn without-ns-docstring
  "`source` with its ns docstring removed, or `source` unchanged when it
   declares none.

   Contract: the result differs from `source` only by the deletion of one
   contiguous span, and reads as the same ns form minus its :doc."
  [source]
  (if-let [[start end] (ns-docstring-span source)]
    (str (subs source 0 start) (subs source end))
    source))

;; ── defprotocol ───────────────────────────────────────────────────────────
;; `defprotocol` records each method's :name, :arglists and :doc in the :sigs
;; map, which is the protocol var's VALUE. :elide-meta reaches def METADATA, so
;; the doc string is compiled into the generated __init class verbatim and ships
;; in every AOT jar. The only place to remove it is the staged source.

(defn- list-opens
  "Indexes of every `(` in `source` that opens a form.

   String literals, character literals and `;` comments are skipped, so a paren
   inside text is never mistaken for a form."
  [^String s]
  (let [n (.length s)]
    (loop [i 0, acc []]
      (if (>= i n)
        acc
        (let [c (.charAt s i)]
          (cond
            (= \" c) (recur (or (string-end s i) n) acc)
            (= \; c) (let [nl (.indexOf s "\n" (int i))]
                       (recur (if (neg? nl) n (inc nl)) acc))
            (= \\ c) (recur (min n (+ i 2)) acc)
            (= \( c) (recur (inc i) (conj acc i))
            :else    (recur (inc i) acc)))))))

(defn- child-spans
  "`[start end)` of each form directly inside the list opening at `i`, or nil
   when the list is unterminated."
  [^String s i]
  (when-let [close (delimited-end s i \))]
    (loop [j (skip-blanks s (inc i)), acc []]
      (if (>= j (dec close))
        acc
        (if-let [e (form-end s j)]
          (recur (skip-blanks s e) (conj acc [j e]))
          acc)))))

(defn- string-span?
  [^String s [start _end]]
  (= \" (.charAt s start)))

(defn- sig-docstring-span
  "`[start end)` of the docstring closing the method signature at `i`, or nil.

   A signature is `(name [args]+ \"doc\"?)`, so only a TRAILING string is a
   docstring: a string in any other position is an argument default or a form
   this pass must not touch."
  [^String s i]
  (let [kids (child-spans s i)]
    (when (> (count kids) 1)
      (let [last-kid (peek kids)]
        (when (and (string-span? s last-kid)
                   (not (string-span? s (first kids))))
          last-kid)))))

(defn protocol-docstring-spans
  "`[start end)` of every docstring inside a `defprotocol` form in `source`:
   the protocol's own, and one per method signature. Ascending, disjoint.

   Refuses rather than guesses: a form it cannot parse contributes nothing and
   is staged unchanged."
  [^String source]
  (let [s source]
    (into []
          (comp
           (keep (fn [open]
                   (let [kids (child-spans s open)
                         [hs he] (first kids)]
                     (when (and hs (= "defprotocol" (subs s hs he)))
                       kids))))
           (mapcat (fn [kids]
                     ;; kids: defprotocol, Name, doc?, sig, sig, ...
                     (let [after-name (nthrest kids 2)
                           own-doc (when-let [k (first after-name)]
                                     (when (string-span? s k) k))
                           sigs (if own-doc (rest after-name) after-name)]
                       (into (if own-doc [own-doc] [])
                             (keep (fn [[start _end]]
                                     (when (= \( (.charAt s start))
                                       (sig-docstring-span s start))))
                             sigs)))))
          (list-opens s))))

(defn- without-spans
  "`source` with each `[start end)` in `spans` deleted. Deleted back to front,
   so an earlier span's indexes stay valid."
  [^String source spans]
  (reduce (fn [acc [start end]] (str (subs acc 0 start) (subs acc end)))
          source
          (sort-by first > spans)))

(defn without-protocol-docstrings
  "`source` with every `defprotocol` docstring removed.

   Contract: the result differs from `source` only by deletions, and reads as
   the same forms minus their doc strings. Arglists and method names stay,
   being structural: the protocol does not dispatch without them."
  [source]
  (without-spans source (protocol-docstring-spans source)))

(defn without-docstrings
  "`source` with both classes of :elide-meta-proof docstring removed: the ns
   form's, and every `defprotocol`'s."
  [source]
  (-> source without-ns-docstring without-protocol-docstrings))

(m/=> clojure-source? [:=> [:cat :string] :boolean])
(m/=> ns-docstring-span [:=> [:cat :string] [:maybe [:tuple :int :int]]])
(m/=> without-ns-docstring [:=> [:cat :string] :string])
(m/=> protocol-docstring-spans [:=> [:cat :string] [:vector [:tuple :int :int]]])
(m/=> without-protocol-docstrings [:=> [:cat :string] :string])
(m/=> without-docstrings [:=> [:cat :string] :string])
