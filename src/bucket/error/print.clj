(ns bucket.error.print
  (:require [bin.format :as format]
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
  "Write error details to a file.

   Args:
   - error: [exception-or-nil stacktrace-or-nil]
   - dir: optional output directory (default: ./errors)
   - name: optional base filename (without extension)
   - timestamp?: boolean, whether to prepend timestamp to filename (default: true)

   Filename generation:
   - If both name and timestamp: <timestamp>-<n>.log
   - If only timestamp: <timestamp>.log
   - If only name: <n>.log
   - If neither: error.log"
  [error & {:keys [dir name timestamp?]
            :or {dir "errors" timestamp? true}}]
  (let [dir-file (io/file dir)]
    (.mkdirs dir-file))
  (let [filename (format/filename name :timestamp? timestamp? :type "error" :ext "log")
        filepath (str dir "/" filename)
        [err stacktrace] error]
    (when err
      (spit filepath
            (str "error class: " (class err) "\n"
                 "error message: " (ex-message err) "\n"
                 (when-let [cause (ex-cause err)]
                   (str "error cause: " cause "\n"))
                 (when stacktrace
                   (str "stacktrace:\n" stacktrace "\n")))))))