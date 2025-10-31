(ns bucket.log
  "Logging helpers for building and printing log entries.

   Log entry format: {:indent int :time inst :level keyword :value string}
   - :indent: integer indentation level
   - :time: java.time.Instant timestamp
   - :level: one of :debug :info :warning :error :critical
   - :value: string message content"
  (:refer-clojure :exclude [filter print])
  (:require [bin.format :as format]
            [bucket.log.entry :as entry]
            [bucket.log.protocol :as protocol]
            [bucket.log.print :as print]
            [bucket.log.filter :as filter])
  (:import [java.time Instant]))

(defn print
  "Print log entries according to output mode.

   Args:
   - sink: log vector or Bucket
   - out: output mode - :none, :stdout, :file, or :both (default :both)
   - dir: optional output directory for :file or :both modes (default: ./logs)
   - name: optional base filename (without extension) for :file or :both modes
   - timestamp?: boolean, whether to prepend timestamp to filename (default: true)

   Output modes:
   - :none   - Don't output logs anywhere
   - :stdout - Print logs to stdout only
   - :file   - Write logs to .log file only
   - :both   - Print to stdout AND write to .log file

   Filename generation:
   - If both name and timestamp?: <timestamp>-<n>.log
   - If only timestamp?: <timestamp>.log
   - If only name: <n>.log
   - If neither: logs.log

   Each log entry is printed with appropriate formatting."
  [sink & {:keys [out dir name timestamp?]
           :or {out :both}}]
  (let [logs (protocol/-current-logs sink)
        formatted-logs (map format/log-text logs)
        file-opts (cond-> {}
                    dir (assoc :dir dir)
                    name (assoc :name name)
                    (some? timestamp?) (assoc :timestamp? timestamp?))]
    (case out
      :none nil
      :stdout (print/->stdout formatted-logs)
      :file (apply print/->file formatted-logs (apply concat file-opts))
      :both (do
              (print/->stdout formatted-logs)
              (apply print/->file formatted-logs (apply concat file-opts))))))

(defn filter
  "Filter log entries according to the requested mode.

   Accepts either a log vector or a Bucket (or anything satisfying `LogSink`).

   Keyword arguments:
   - :mode  - filtering mode (:level, :indent, :time, :value). Defaults to :level.
   - :type  - comparison operator. Supported per mode:
       :level/:indent -> :lte, :gte, :eq (default :lte)
       :time          -> :lte, :gte, :eq (default :lte)
       :value         -> :eq, :neq (default :eq)
   - :value - comparison target. Supported per mode:
       :level  -> log level keyword (default :debug)
       :indent -> integer indent (default 4)
       :time   -> Instant or millisecond timestamp (default current millis)
       :value  -> regex (default #\"(?i:error)\")

   Returns: sink with filtered logs (vector in, vector out; bucket in, bucket out)."
  [sink & {:keys [mode type value]}]
  (let [mode (or mode :level)
        defaults (case mode
                   :level {:type :lte :value :debug}
                   :indent {:type :lte :value 4}
                   :time {:type :lte :value (inst-ms (Instant/now))}
                   :value {:type :eq :value #"(?i:error)"}
                   {:type :lte :value :debug})
        type (or type (:type defaults))
        value (or value (:value defaults))
        logs (protocol/-current-logs sink)
        filtered (case mode
                   :level (filter/level logs type value)
                   :indent (filter/indent logs type value)
                   :time (filter/time logs type value)
                   :value (filter/value logs type value)
                   logs)]
    (protocol/-with-logs sink filtered)))

(defn log
  "Add a log entry to a destination.

   The destination is always the first positional argument and can be either:
   - a log vector
   - a Bucket map (or any map with a :logs vector)

   The message can be either positional (second arg) or keywordized (:value).
   Any additional parameters must use keywords.

   Args:
   - sink: log vector or Bucket map (first, positional)
   - message: log message string (positional or :value keyword)
   - :level - log level keyword (default :info)
   - :indent - indentation level (default: same as last entry or 0)
   - :check-secrets - whether to check for password-like content (default false)

   Examples:
     (log logs \"Hello\")                                  ; positional message only
     (log logs \"Hello\" :level :warning)                  ; positional message + keyword args
     (log logs \"Hello\" :level :error :indent 2)          ; positional message + multiple keywords
     (log logs :value \"Hello\")                           ; all keyword args
     (log logs :value \"Hello\" :level :warning :indent 2) ; all keyword args
     (log logs :value \"password123\" :check-secrets true) ; with password check

   Returns: updated destination (vector or Bucket) with new log entry appended"
  ([sink message]
   (entry/append sink {:value message}))
  ([sink a b & rest]
   (let [opts (entry/->log-opts a b rest)]
     (entry/append sink opts))))

(defn debug
  "Add a debug log entry to a vector or bucket sink.

   Accepts the same keyword options as `log`, including :indent, :check-secrets,
   and :indent-next."
  ([sink message]
   (entry/append-level :debug sink message))
  ([sink a b & rest]
   (apply entry/append-level :debug sink a b rest)))

(defn info
  "Add an info log entry to a vector or bucket sink.

   Accepts the same keyword options as `log`, including :indent, :check-secrets,
   and :indent-next."
  ([sink message]
   (entry/append-level :info sink message))
  ([sink a b & rest]
   (apply entry/append-level :info sink a b rest)))

(defn warning
  "Add a warning log entry to a vector or bucket sink.

   Accepts the same keyword options as `log`, including :indent, :check-secrets,
   and :indent-next."
  ([sink message]
   (entry/append-level :warning sink message))
  ([sink a b & rest]
   (apply entry/append-level :warning sink a b rest)))

(defn error
  "Add an error log entry to a vector or bucket sink.

   Accepts the same keyword options as `log`, including :indent, :check-secrets,
   and :indent-next."
  ([sink message]
   (entry/append-level :error sink message))
  ([sink a b & rest]
   (apply entry/append-level :error sink a b rest)))

(defn critical
  "Add a critical log entry to a vector or bucket sink.

   Accepts the same keyword options as `log`, including :indent, :check-secrets,
   and :indent-next."
  ([sink message]
   (entry/append-level :critical sink message))
  ([sink a b & rest]
   (apply entry/append-level :critical sink a b rest)))
