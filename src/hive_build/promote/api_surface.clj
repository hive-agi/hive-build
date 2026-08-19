(ns hive-build.promote.api-surface
  "A namespace's PUBLIC surface as data, and the difference between two of them.

   Pure: takes already-read top-level forms, returns values.

   Surface shape:
     {\"ns/name\" {:kind :def|:defn|:defmacro|:defprotocol|:defrecord
                  :arities #{0 1 :variadic}
                  :methods {\"method\" #{1 2}}}}

   What counts as public: anything a downstream repo can compile against —
   `defn`/`def`/`defmacro` without ^:private (and not `defn-`), plus every
   protocol and its method arities. Private vars are excluded on purpose: they
   are not part of the contract and freezing them would freeze refactoring."
  (:require [clojure.string :as str]))

(def ^:private public-def-forms
  '#{def defn defmacro defprotocol defrecord deftype definterface defmulti})

(defn- private-form?
  [head sym meta-map]
  (or (= 'defn- head)
      (:private meta-map)
      (:private (meta sym))))

(defn- arity-of
  "An argument vector's arity: its fixed count, or :variadic when it has an &."
  [argv]
  (if (some #{'&} argv)
    :variadic
    (count argv)))

(defn- fn-arities
  "Arities declared by a defn body: single `[args]` or several `([args] ...)`."
  [body]
  (let [vecs (cond
               (some vector? body)
               [(first (filter vector? body))]

               :else
               (keep (fn [form] (when (and (seq? form) (vector? (first form)))
                                  (first form)))
                     body))]
    (into #{} (map arity-of) vecs)))

(defn- protocol-methods
  "{method-name #{arities}} for a defprotocol body."
  [body]
  (reduce (fn [acc form]
            (if (and (seq? form) (symbol? (first form)))
              (let [mname (name (first form))
                    argvs (filter vector? (rest form))]
                (update acc mname (fnil into #{}) (map arity-of argvs)))
              acc))
          {}
          body))

(defn- unwrap
  "Forms to consider, flattening the `(do ...)` / `(defonce _ (do ...))` wrappers
   the contract repos use to make protocol definitions reload-safe."
  [form]
  (cond
    (not (seq? form)) []
    (= 'do (first form)) (mapcat unwrap (rest form))
    (and (= 'defonce (first form)) (seq (drop 2 form))) (mapcat unwrap (drop 2 form))
    :else [form]))

(defn- form->entry
  [ns-name form]
  (let [[head sym & body] form]
    (when (and (symbol? head) (symbol? sym) (contains? public-def-forms head))
      (let [meta-map (if (map? (first body)) (first body) {})]
        (when-not (private-form? head sym meta-map)
          (let [qn (str ns-name "/" (name sym))]
            (case head
              defprotocol   [qn {:kind :defprotocol
                                 :methods (protocol-methods body)}]
              (defn defmacro) [qn {:kind (keyword (name head))
                                   :arities (fn-arities body)}]
              [qn {:kind (keyword (name head))}])))))))

(defn ns-name-of
  "The namespace symbol declared by `forms`, or nil."
  [forms]
  (some (fn [form]
          (when (and (seq? form) (= 'ns (first form)) (symbol? (second form)))
            (second form)))
        forms))

(defn surface
  "Public API surface of one namespace's `forms`."
  [forms]
  (if-let [nsym (ns-name-of forms)]
    (into {}
          (comp (mapcat unwrap)
                (keep #(form->entry nsym %)))
          forms)
    {}))

(defn merge-surfaces
  "One surface map for a whole source tree."
  [surfaces]
  (reduce merge {} surfaces))

;; =============================================================================
;; The difference that matters
;; =============================================================================

(defn- lost-arities
  [old new]
  (let [o (:arities old) n (:arities new)]
    (when (and (seq o) (seq n))
      (seq (remove n o)))))

(defn- lost-methods
  [old new]
  (let [o (:methods old) n (:methods new)]
    (concat
     (for [[m _] o :when (not (contains? n m))]
       {:method m :change :removed})
     (for [[m arities] o
           :let [now (get n m)]
           :when (and now (seq (remove now arities)))]
       {:method m :change :arities-lost :lost (vec (sort-by str (remove now arities)))}))))

(defn diff
  "What changed from `old` surface to `new`.

   {:removed [qn...]            — a public name that no longer exists
    :arities-lost [{...}]       — a public fn that dropped an arity
    :methods-lost [{...}]       — a protocol method removed or narrowed
    :kind-changed [{...}]       — e.g. a defn became a def
    :added [qn...]}             — additive; never a violation"
  [old new]
  {:removed      (vec (sort (remove #(contains? new %) (keys old))))
   :added        (vec (sort (remove #(contains? old %) (keys new))))
   :arities-lost (vec (for [[qn o] old
                            :let [n (get new qn)
                                  lost (when n (lost-arities o n))]
                            :when (seq lost)]
                        {:qn qn :lost (vec (sort-by str lost))}))
   :methods-lost (vec (for [[qn o] old
                            :let [n (get new qn)]
                            :when (and n (= :defprotocol (:kind o)))
                            m (lost-methods o n)]
                        (assoc m :qn qn)))
   :kind-changed (vec (for [[qn o] old
                            :let [n (get new qn)]
                            :when (and n (not= (:kind o) (:kind n)))]
                        {:qn qn :was (:kind o) :now (:kind n)}))})

(defn breaking?
  "True when `diff` contains anything a consumer could not survive."
  [diff]
  (boolean (some seq [(:removed diff) (:arities-lost diff)
                      (:methods-lost diff) (:kind-changed diff)])))

(defn describe
  "One line per breaking change, for a human reading CI output."
  [diff]
  (concat
   (for [qn (:removed diff)] (str "REMOVED  " qn))
   (for [{:keys [qn lost]} (:arities-lost diff)]
     (str "ARITY    " qn " lost " (str/join ", " lost)))
   (for [{:keys [qn method change lost]} (:methods-lost diff)]
     (str "PROTOCOL " qn "/" method " " (name change)
          (when lost (str " " (str/join ", " lost)))))
   (for [{:keys [qn was now]} (:kind-changed diff)]
     (str "KIND     " qn " " (name was) " -> " (name now)))))
