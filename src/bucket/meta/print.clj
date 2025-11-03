(ns bucket.meta.print
  (:require [bin.format :as format]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]))

(defn ->stdout
  "Print bucket metadata to stdout with a header."
  [bucket]
  (println "\n=== Bucket Metadata ===")
  (pp/pprint (:meta bucket)))

(defn ->file
  "Write bucket metadata to a file and notify path.

   Args:
   - bucket: bucket containing metadata to write
   - dir: optional output directory (defaults to ./meta, or out/<bucket-name> when the bucket has a name)
   - name: optional base filename (without extension)
   - timestamp?: boolean, whether to prepend timestamp to filename (default: true)

   Filename generation:
   - If both name and timestamp?: <timestamp>-<n>.edn
   - If only timestamp?: <timestamp>.edn
   - If only name: <n>.edn
   - If neither: meta.edn"
  [bucket & {:keys [dir name timestamp?]
             :or {timestamp? true}}]
  (let [bucket-name (:name bucket)
        default-dir (if bucket-name (str "out/" bucket-name) "meta")
        chosen-dir (or dir default-dir)
        dir-file (io/file chosen-dir)]
    (.mkdirs dir-file)
    (let [filename (format/filename (str name "-meta") :timestamp? timestamp? :type "meta" :ext "edn")
          filepath (str chosen-dir "/" filename)]
      (spit filepath (with-out-str (pp/pprint (:meta bucket))))
      (println (str "Metadata written to: " filepath))))
  bucket)
