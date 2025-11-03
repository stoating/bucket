(ns bucket.print
  (:require [bin.format :as format]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]))

(defn ->stdout
  "Print the entire bucket map to stdout in a readable form."
  [bucket]
  (println "\n=== Bucket ===")
  (pp/pprint bucket))

(defn ->file
  "Write the entire bucket map to a file, defaulting to out/<bucket-name>/bucket.edn when bucket named."
  [bucket & {:keys [dir name timestamp?]
             :or {timestamp? true}}]
  (let [bucket-name (:name bucket)
        default-dir (if bucket-name (str "out/" bucket-name) "bucket")
        chosen-dir (or dir default-dir)
        dir-file (io/file chosen-dir)]
    (.mkdirs dir-file)
    (let [filename (format/filename (str name "-bucket") :timestamp? timestamp? :type "bucket" :ext "edn")
          filepath (str chosen-dir "/" filename)]
      (spit filepath (with-out-str (pp/pprint bucket)))
      (println (str "Bucket written to: " filepath))))
  bucket)
