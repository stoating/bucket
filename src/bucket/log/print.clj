(ns bucket.log.print
  (:require [bin.format :as format]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn ->stdout
  "Print a sequence of pre-formatted log lines to stdout."
  [formatted-logs]
  (doseq [formatted formatted-logs]
    (println formatted)))

(defn ->file
  "Write a sequence of pre-formatted log lines to a file.

   Args:
   - logs: sequence of formatted log strings
   - dir: optional output directory (default: ./logs)
   - name: optional base filename (without extension)
   - timestamp?: boolean, whether to prepend timestamp to filename (default: true)

   Filename generation:
   - If both name and timestamp?: <timestamp>-<n>.log
   - If only timestamp?: <timestamp>.log
   - If only name: <n>.log
   - If neither: logs.log"
  [logs & {:keys [dir name timestamp?]
           :or {dir "logs" timestamp? true}}]
  (let [dir-file (io/file dir)]
    (.mkdirs dir-file))
  (let [filename (format/filename name :timestamp? timestamp? :type "logs" :ext "log")
        filepath (str dir "/" filename)]
    (spit filepath (str/join "\n" logs))))
