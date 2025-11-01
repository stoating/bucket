(ns bucket.log.print
  (:require [bin.format :as format]
            [bucket.log.protocol :as protocol]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn ->stdout
  "Print logs from a sink (vector, bucket, etc.) to stdout."
  [sink]
  (let [logs (protocol/-current-logs sink)
        formatted (map format/log-text logs)]
    (doseq [line formatted]
      (println line))
    (protocol/-with-logs sink logs)))

(defn ->file
  "Write logs from a sink (vector, bucket, etc.) to a file.

   Args:
   - sink: any LogSink (log vector, bucket map, nil)
   - dir: optional output directory (defaults to ./logs, or out/<bucket-name> when the sink is a bucket with a name)
   - name: optional base filename (without extension)
   - timestamp?: boolean, whether to prepend timestamp to filename (default: true)

   Filename generation:
   - If both name and timestamp?: <timestamp>-<n>.log
   - If only timestamp?: <timestamp>.log
   - If only name: <n>.log
   - If neither: logs.log

   Returns the sink unchanged."
  [sink & {:keys [dir name timestamp?]
           :or {timestamp? true}}]
  (let [logs (protocol/-current-logs sink)
        formatted (map format/log-text logs)
        bucket-name (when (map? sink) (:name sink))
        default-dir (if bucket-name (str "out/" bucket-name) "logs")
        chosen-dir (or dir default-dir)
        dir-file (io/file chosen-dir)]
    (.mkdirs dir-file)
    (let [filename (format/filename name :timestamp? timestamp? :type "logs" :ext "log")
          filepath (str chosen-dir "/" filename)]
      (spit filepath (str/join "\n" formatted)))
    (protocol/-with-logs sink logs)))
