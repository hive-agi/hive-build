(ns hive-build.boundary.archive
  "Jar file effects: deterministic rewriting, and selective class extraction.

   The decisions these functions act on live in hive-build.promote.naming; what
   is here is only the IO."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-build.promote.naming :as naming]
            [hive-build.promote.reproducible :as repro])
  (:import (java.util.zip ZipEntry ZipFile ZipOutputStream)))

(def zip-epoch
  "1980-01-01T00:00:00Z — the earliest instant the ZIP format can represent."
  315532800000)

(defn entries
  "{entry-name -> bytes} for every entry of the zip at `path`, in name order."
  [path]
  (with-open [zf (ZipFile. (io/file path))]
    (into (sorted-map)
          (map (fn [^ZipEntry e]
                 [(.getName e)
                  (with-open [in (.getInputStream zf e)] (.readAllBytes in))]))
          (enumeration-seq (.entries zf)))))

(defn normalized-entries
  "`name->bytes` with every entry a reproducibility rule claims rewritten.
   Entries no rule claims are passed through byte for byte, never decoded."
  [name->bytes]
  (reduce-kv (fn [acc entry-name ^bytes content]
               (assoc acc entry-name
                      (if (repro/nondeterministic? entry-name)
                        (.getBytes (repro/normalize-text entry-name (String. content "UTF-8"))
                                   "UTF-8")
                        content)))
             (sorted-map)
             name->bytes))

(defn write-zip!
  "Write `name->bytes` to `path`, in name order, every entry stamped
   `zip-epoch`."
  [path name->bytes]
  (with-open [out (ZipOutputStream. (io/output-stream path))]
    (doseq [[entry-name ^bytes content] (sort-by key name->bytes)]
      (.putNextEntry out (doto (ZipEntry. ^String entry-name) (.setTime zip-epoch)))
      (.write out content)
      (.closeEntry out)))
  path)

(defn normalize-jar!
  "Rewrite the jar at `path` in place: entries sorted by name, every entry
   stamped `zip-epoch`, and every entry a reproducibility rule claims rewritten.

   Contract: two builds of identical inputs produce byte-identical jars, so any
   difference between two copies of an artifact is content."
  [path]
  (let [src (io/file path)
        tmp (io/file (str path ".norm"))]
    (write-zip! tmp (normalized-entries (entries src)))
    (io/copy tmp src)
    (.delete tmp)
    path))

(defn- relative-path
  [^java.nio.file.Path root ^java.io.File f]
  (-> (.relativize root (.toPath f)) str (str/replace java.io.File/separator "/")))

(defn copy-own-classes!
  "Copy every class under `from` that `naming/own-class?` accepts for
   `prefixes` into `to`, preserving relative paths. Returns the paths copied."
  [from to prefixes]
  (let [root (io/file from)
        root-path (.toPath root)]
    (into []
          (keep (fn [^java.io.File f]
                  (when (.isFile f)
                    (let [rel (relative-path root-path f)]
                      (when (naming/own-class? prefixes rel)
                        (let [dest (io/file to rel)]
                          (io/make-parents dest)
                          (io/copy f dest)
                          rel))))))
          (file-seq root))))
