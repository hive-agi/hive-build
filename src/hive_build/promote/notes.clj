(ns hive-build.promote.notes
  "A release note, derived from the commits between two published versions. Pure.

  The single definition of what a hive release note IS. Two projections read
  it: the CHANGELOG.md this library generates at release time, and the note
  hive-store shows a subscriber. Neither authors it, so neither can disagree
  with the other or with git.

  It can be derived at all because two facts already hold:

    - a hive version names exactly one commit, through its tag
    - every published pom records that commit in `<scm><tag>`

  What a READER is shown is not the whole log. A range of sixty commits is
  typically half housekeeping, and printing it all buries the seventeen fixes
  among thirty dependency bumps. The kinds a reader deciding whether to bump
  cares about are published; the rest are counted."
  (:require [clojure.string :as str]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def published-kinds
  "The conventional-commit types a release note shows, in reading order, and
  the heading each becomes.

  Ordered by what a reader deciding whether to bump wants first: what is new,
  what is fixed, what got faster, what moved. The headings are Keep a Changelog
  1.1.0 headings, so the same value renders as a changelog section."
  [["feat" "Added"]
   ["fix" "Fixed"]
   ["perf" "Faster"]
   ["refactor" "Changed"]])

(def counted-kinds
  "Types that are real work and are not news to somebody outside the repo.

  Counted rather than dropped: a range with forty of these and no feature is
  itself worth seeing, and reporting zero commits for it would be a lie."
  #{"docs" "test" "build" "ci" "chore" "style" "revert"})

(def ^:private subject-pattern
  #"^([a-z]+)(?:\(([^)]*)\))?(!)?:\s+(\S.*)$")

(defn subject-of
  "The first line of a commit message."
  [message]
  (first (str/split-lines (str message))))

(defn breaking-footer?
  "Whether `message` declares a break in its body the way the convention says.

  A `!` in the subject is the short spelling and is read separately. This is
  the long one, and a commit that uses only the footer would otherwise be
  published as an ordinary feature."
  [message]
  (boolean (re-find #"(?m)^BREAKING[ -]CHANGE:" (str message))))

(defn commit
  "One commit message as a parsed note entry, or nil when it is not a
  conventional commit.

  nil rather than a bucket for unrecognised subjects: they are counted by the
  caller, and inventing a kind for them would put an arbitrary heading over
  somebody's prose."
  [message]
  (let [line (subject-of message)]
    (when-let [[_ kind scope bang summary] (re-matches subject-pattern line)]
      {:note/kind kind
       :note/scope (not-empty (str scope))
       :note/breaking? (or (some? bang) (breaking-footer? message))
       :note/summary summary})))

(defn- entries-of [messages]
  (into [] (keep commit) messages))

(defn- section
  [entries [kind heading]]
  (when-let [matching (not-empty (filterv #(= kind (:note/kind %)) entries))]
    {:section/heading heading
     :section/kind kind
     :section/entries matching}))

(defn of-commits
  "The commits between two releases, as the note a reader reads.

  `:note/sections` are the published kinds that have anything in them, in
  reading order. `:note/breaking` lifts every breaking change to the top
  whatever its kind, because a reader deciding whether to bump has exactly one
  question that outranks the rest.

  `:note/routine` counts the housekeeping and `:note/unconventional` the
  subjects that follow no convention. Both are numbers rather than lists, and
  both are reported: a range that is entirely dependency bumps must look like
  one rather than like an empty release."
  [messages]
  (let [entries (entries-of messages)
        known (set (map first published-kinds))
        routine (filterv #(contains? counted-kinds (:note/kind %)) entries)]
    {:note/sections (into [] (keep #(section entries %)) published-kinds)
     :note/breaking (filterv :note/breaking? entries)
     :note/commits (count messages)
     :note/routine (count routine)
     :note/unconventional (- (count messages)
                             (count (filterv #(or (contains? known (:note/kind %))
                                                  (contains? counted-kinds (:note/kind %)))
                                             entries)))}))

(defn newsworthy?
  "Whether this note has anything in it a reader would read.

  A range of pure housekeeping is a real answer and is drawn as one, but it is
  not a headline, and a page can use this to decide which of the two it is
  looking at."
  [note]
  (boolean (or (seq (:note/breaking note)) (seq (:note/sections note)))))

(def Entry
  [:map {:closed true}
   [:note/kind :string]
   [:note/scope [:maybe :string]]
   [:note/breaking? :boolean]
   [:note/summary :string]])

(def Note
  [:map {:closed true}
   [:note/sections [:sequential [:map {:closed true}
                                 [:section/heading :string]
                                 [:section/kind :string]
                                 [:section/entries [:sequential Entry]]]]]
   [:note/breaking [:sequential Entry]]
   [:note/commits [:int {:min 0}]]
   [:note/routine [:int {:min 0}]]
   [:note/unconventional [:int {:min 0}]]])

(m/=> subject-of [:=> [:cat [:maybe :string]] :string])
(m/=> breaking-footer? [:=> [:cat [:maybe :string]] :boolean])
(m/=> commit [:=> [:cat [:maybe :string]] [:maybe Entry]])
(m/=> of-commits [:=> [:cat [:sequential [:maybe :string]]] Note])
(m/=> newsworthy? [:=> [:cat :map] :boolean])
