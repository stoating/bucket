(ns bucket.meta.temp
  (:require [bin.format :as format]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]))

(defn meta-filename
  "Generate filename for metadata file based on timestamp and name options.

   Args:
   - name: optional base filename (without extension)
   - timestamp: boolean, whether to prepend timestamp to filename

   Returns: filename string with .edn extension"
  [name timestamp]
  (let [ts (when timestamp (format/filename-timestamp))]
    (cond
      (and ts name) (str ts "-" name ".edn")
      ts (str ts ".edn")
      name (str name ".edn")
      :else "meta.edn")))

(defn print-meta-to-stdout
  "Print bucket metadata to stdout with a header."
  [meta]
  (println "\n=== Bucket Metadata ===")
  (pp/pprint meta))

(defn print-meta-to-file
  "Write bucket metadata to a file and notify path.

   Args:
   - meta: metadata map to write
   - dir: optional output directory (default: ./meta)
   - name: optional base filename (without extension)
   - timestamp: boolean, whether to prepend timestamp to filename (default: true)

   Filename generation:
   - If both name and timestamp: <timestamp>-<n>.edn
   - If only timestamp: <timestamp>.edn
   - If only name: <n>.edn
   - If neither: meta.edn"
  [meta & {:keys [dir name timestamp]
           :or {dir "meta" timestamp true}}]
  (let [dir-file (io/file dir)]
    (.mkdirs dir-file))
  (let [filename (meta-filename name timestamp)
        filepath (str dir "/" filename)]
    (spit filepath (with-out-str (pp/pprint meta)))
    (println (str "Metadata written to: " filepath))))
