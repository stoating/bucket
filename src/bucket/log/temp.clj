(ns bucket.log.temp
  (:require [bin.format :as format]
            [bucket.log.protocol :as protocol]
            [bucket.log.secret :as secret]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.time Instant]))

(defn make-entry
  "Create a log entry map.

   Supports both positional and keyword arguments:

   Single-arity form (positional value):
   - value: log message string (defaults: level :info, indent 0)

   Keyword arguments form:
   - :value - log message string (required when using keywords)
   - :level - one of :debug :info :warning :error :critical (default :info)
   - :indent - indentation level (default 0)

   Multi-arity form (variadic):
   - First arg not a keyword: treated as positional value, rest are keyword args
   - First arg is a keyword: all args are keyword args

   Examples:
     (make-entry \"Hello\")                              ; positional value only
     (make-entry \"Hello\" :level :warning)              ; positional value + keyword args
     (make-entry \"Hello\" :level :warning :indent 2)    ; positional value + keyword args
     (make-entry :value \"Hello\")                       ; keyword: value only
     (make-entry :value \"Hello\" :level :warning)       ; keyword: value + level
     (make-entry :value \"Hello\" :level :warning :indent 2)  ; keyword: all three

   Returns: {:indent int :time Long :level keyword :value string}"
  ([value]
   (make-entry :value value))
  ([a b & rest]
   (let [args (if (odd? (+ 2 (count rest)))
                (apply hash-map :value a b rest)
                (apply hash-map a b rest))
         {:keys [value level indent]} args]
     {:indent (or indent 0)
      :time (inst-ms (Instant/now))
      :level (or level :info)
      :value value})))

(defn print-log-to-stdout
  "Print a sequence of pre-formatted log lines to stdout."
  [formatted-logs]
  (doseq [formatted formatted-logs]
    (println formatted)))

(defn print-log-to-file
  "Write a sequence of pre-formatted log lines to a file.

   Args:
   - logs: sequence of formatted log strings
   - dir: optional output directory (default: ./logs)
   - name: optional base filename (without extension)
   - timestamp: boolean, whether to prepend timestamp to filename (default: true)

   Filename generation:
   - If both name and timestamp: <timestamp>-<n>.log
   - If only timestamp: <timestamp>.log
   - If only name: <n>.log
   - If neither: logs.log"
  [logs & {:keys [dir name timestamp]
           :or {dir "logs" timestamp true}}]
  (let [dir-file (io/file dir)]
    (.mkdirs dir-file))
  (let [ts (when timestamp (format/filename-timestamp))
        filename (cond
                   (and ts name) (str ts "-" name ".log")
                   ts (str ts ".log")
                   name (str name ".log")
                   :else "logs.log")
        filepath (str dir "/" filename)]
    (spit filepath (str/join "\n" logs))))

(defn ->log-opts
  "Convert mixed positional/keyword arguments into a unified options map."
  [a b rest]
  (if (odd? (+ 2 (count rest)))
    (apply hash-map :value a b rest)
    (apply hash-map a b rest)))

(defn append-log-entry
  "Internal shared implementation for appending a log entry to a sink."
  [sink {:keys [value level indent check-secrets indent-next]}]
  (let [logs (protocol/-current-logs sink)
        last-entry (peek logs)
        fallback-indent (cond
                          (nil? last-entry) 0
                          (contains? last-entry :indent-next) (:indent-next last-entry)
                          :else (:indent last-entry))
        actual-indent (or indent fallback-indent)
        actual-message (if (and check-secrets (secret/likely-secret? value))
                         "* log redacted *"
                         value)
        base-entry (make-entry :value actual-message
                               :level (or level :info)
                               :indent actual-indent)
        entry (cond-> base-entry
                (some? indent-next) (assoc :indent-next indent-next))
        updated-logs (conj logs entry)]
    (protocol/-with-logs sink updated-logs)))

(defn log-with-level
  "Internal helper used by level-specific logging functions.

   Mirrors the `log` calling conventions while forcing the :level key."
  ([level sink message]
   (append-log-entry sink {:value message :level level}))
  ([level sink a b & rest]
   (let [opts (->log-opts a b rest)
         opts-with-level (assoc opts :level level)]
     (append-log-entry sink opts-with-level))))