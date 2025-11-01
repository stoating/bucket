(ns bucket.error.print
  (:require [bin.format :as format]
            [bucket.error.protocol :as protocol]
            [clojure.java.io :as io]))

(defn ->stdout
  "Print error details to stdout."
  [error]
  (let [[err stacktrace] error]
    (when err
      (println (str "error class: " (class err)))
      (println (str "error message: " (ex-message err)))
      (when-let [cause (ex-cause err)]
        (println (str "error cause: " cause)))
      (when stacktrace
        (println "stacktrace:")
        (println stacktrace)))))

(defn ->file
  "Write error details from an ErrorSink (vector, bucket, etc.) to a file.

   Args:
   - sink: any ErrorSink (error vector, bucket map, nil)
   - dir: optional output directory (default: ./errors)
   - name: optional base filename (without extension)
   - timestamp?: boolean, whether to prepend timestamp to filename (default: true)

   Filename generation:
   - If both name and timestamp: <timestamp>-<n>.log
   - If only timestamp: <timestamp>.log
   - If only name: <n>.log
   - If neither: error.log

   Returns the sink unchanged."
  [sink & {:keys [dir name timestamp?]
           :or {dir "errors" timestamp? true}}]
  (let [[err stacktrace :as error] (protocol/-current-error sink)]
    (when err
      (let [dir-file (io/file dir)
            filename (format/filename name :timestamp? timestamp? :type "error" :ext "log")
            filepath (str dir "/" filename)
            parts [(str "error class: " (class err) "\n")
                   (str "error message: " (ex-message err) "\n")
                   (when-let [cause (ex-cause err)]
                     (str "error cause: " cause "\n"))
                   (when stacktrace
                     (str "stacktrace:\n" stacktrace "\n"))]
            content (->> parts (remove nil?) (apply str))]
        (.mkdirs dir-file)
        (spit filepath content)))
    (protocol/-with-error sink error)))