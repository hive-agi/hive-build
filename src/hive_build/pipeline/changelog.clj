(ns hive-build.pipeline.changelog
  "Tags plus reads become a Changelog value. Pure.

   Every effect arrives as an injected function, so the whole assembly runs
   against maps in a test and against git in a release."
  (:require [clojure.string :as str]
            [hive-build.promote.notes :as notes]))

(def default-depth
  "How many releases the generated file carries in full.

   A library with four hundred tags would otherwise render four hundred
   sections, and the reader deciding whether to bump is looking at the top."
  25)

(defn version-of
  "The version a v-prefixed tag names."
  [tag]
  (str/replace (str tag) #"^v" ""))

(defn ranges
  "Adjacent tag pairs, newest first.

   `tags` must be newest first and COMPLETE: the oldest one's range opens at
   the root of history, and truncating the input before this would attribute
   the whole history to whatever release happened to be last."
  [tags]
  (let [tags (vec tags)]
    (into []
          (map-indexed (fn [i tag]
                         {:release/version (version-of tag)
                          :range/to tag
                          :range/from (get tags (inc i))}))
          tags)))

(defn assemble
  "Ranges into Releases, reading through the injected fns.

   `messages-fn` [from to] -> commit messages, `date-fn` [tag] -> date string
   or nil, `prose-fn` [version] -> authored markdown or nil."
  [ranges' {:keys [messages-fn date-fn prose-fn]}]
  (into []
        (map (fn [{:release/keys [version] :range/keys [from to]}]
               {:release/version version
                :release/date (when date-fn (date-fn to))
                :release/prose (when prose-fn (prose-fn version))
                :release/note (notes/of-commits (messages-fn from to))}))
        ranges'))

(defn document
  "The Changelog value for `tags` (newest first, complete).

   `:pending` names the release being cut but not yet tagged, and adds it at
   the top with HEAD as its endpoint. Without it a changelog generated during
   a release describes every version EXCEPT the one being released, which is
   the staleness this file exists to avoid.

   `:depth` bounds how many releases are rendered in full; the rest are named
   in the footer rather than dropped silently."
  [tags {:keys [depth preamble pending] :or {depth default-depth} :as reads}]
  (let [tagged (ranges tags)
        all (if (str/blank? (str pending))
              tagged
              (into [{:release/version (version-of pending)
                      :range/to "HEAD"
                      :range/from (first tags)}]
                    tagged))
        shown (into [] (take depth) all)
        omitted (- (count all) (count shown))]
    {:changelog/preamble preamble
     :changelog/footer (when (pos? omitted)
                         (str "_" omitted " older release"
                              (when (not= 1 omitted) "s")
                              " are not listed here; `git tag --list 'v*'` has them all._"))
     :changelog/releases (assemble shown reads)}))
