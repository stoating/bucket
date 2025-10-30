(ns bucket.meta
  "Logging helpers for building and printing log entries.

   Log entry format: {:indent int :time inst :level keyword :value string}
   - :indent: integer indentation level
   - :time: java.time.Instant timestamp
   - :level: one of :debug :info :warning :error :critical
   - :value: string message content"
  (:refer-clojure :exclude [filter print])
  (:require [bucket.meta.print :as print]))

(defn print
  "Print bucket metadata according to output mode.

   Args:
   - meta: metadata map from bucket
   - out: output mode - :none, :stdout, :file, or :both (default :both)
   - dir: optional output directory for :file or :both modes (default: ./meta)
   - name: optional base filename (without extension) for :file or :both modes
   - timestamp?: boolean, whether to prepend timestamp to filename (default: true)

   Output modes:
   - :none   - Don't output metadata anywhere
   - :stdout - Print metadata to stdout only
   - :file   - Write metadata to .edn file only
   - :both   - Print to stdout AND write to .edn file

   Filename generation:
   - If both name and timestamp?: <timestamp>-<n>.edn
   - If only timestamp?: <timestamp>.edn
   - If only name: <n>.edn
   - If neither: meta.edn"
  [meta & {:keys [out dir name timestamp?]
           :or {out :both}}]
  (let [file-opts (cond-> {}
                    dir (assoc :dir dir)
                    name (assoc :name name)
                    (some? timestamp?) (assoc :timestamp? timestamp?))]
    (case out
      :none nil
      :stdout (print/->stdout meta)
      :file (apply print/->file meta (apply concat file-opts))
      :both (do
              (print/->stdout meta)
              (apply print/->file meta (apply concat file-opts))))))