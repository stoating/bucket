(ns bucket.logging
  "Logging helpers for building and printing log entries.

   Log entry format: {:indent int :time inst :level keyword :value string}
   - :indent: integer indentation level
   - :time: java.time.Instant timestamp
   - :level: one of :debug :info :warning :error :critical
   - :value: string message content"
  (:require [bin.format :as fmt]
            [clojure.string :as str]
            [clojure.pprint :as pp]
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

(def sensitive-patterns
  {:exact-matches #{"confidential" "encrypted" "hash" "pin"}
   :contains ["api" "auth" "cert" "cipher" "cred" "key" "pass" "pw" "secret" "sha" "signature" "token"]
   :word-boundaries ["pin" "sec" "sig"]
   :compound-patterns ["api.*key" "api.*val" "key.*val"]
   :url-with-credentials #"(http|https):\/\/.*:(.*)@"})

(defn likely-password?
  "Check if a string likely contains sensitive information."
  [input]
  (let [lower-input (str/lower-case input)]
    (or
     (contains? (:exact-matches sensitive-patterns) lower-input)
     (some #(str/includes? lower-input %)
           (:contains sensitive-patterns))
     (boolean (some #(re-find (re-pattern (str "(?<![a-zA-Z0-9])" % "(?![a-zA-Z0-9])")) lower-input)
                    (:word-boundaries sensitive-patterns)))
     (some #(re-find (re-pattern (str "(?i)" %)) input)
           (:compound-patterns sensitive-patterns))
     (boolean (re-find (:url-with-credentials sensitive-patterns) input)))))

(defprotocol LogSink
  "Protocol for things that can accumulate log entries."
  (-current-logs [sink] "Return the current log vector for the sink.")
  (-with-logs [sink logs] "Return the sink updated with the provided logs vector."))

(defn- ensure-log-vector
  "Normalize a log collection into a vector."
  [logs]
  (cond
    (vector? logs) logs
    (nil? logs) []
    (sequential? logs) (vec logs)
    :else []))

(extend-protocol LogSink
  clojure.lang.IPersistentVector
  (-current-logs [sink] sink)
  (-with-logs [_ logs] logs)

  clojure.lang.IPersistentMap
  (-current-logs [sink] (ensure-log-vector (:logs sink)))
  (-with-logs [sink logs] (assoc sink :logs logs))

  nil
  (-current-logs [_] [])
  (-with-logs [_ logs] logs))

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
   - :check-pass - whether to check for password-like content (default false)

   Examples:
     (log logs \"Hello\")                                    ; positional message only
     (log logs \"Hello\" :level :warning)                    ; positional message + keyword args
     (log logs \"Hello\" :level :error :indent 2)            ; positional message + multiple keywords
     (log logs :value \"Hello\")                             ; all keyword args
     (log logs :value \"Hello\" :level :warning :indent 2)   ; all keyword args
     (log logs :value \"password123\" :check-pass true)      ; with password check

   Returns: updated destination (vector or Bucket) with new log entry appended"
  ([sink message]
   (log sink :value message))
  ([sink a b & rest]
   (let [args (if (odd? (+ 2 (count rest)))
                (apply hash-map :value a b rest)
                (apply hash-map a b rest))
         {:keys [value level indent check-pass indent-next]} args
         logs (-current-logs sink)
         last-entry (peek logs)
         fallback-indent (cond
                           (nil? last-entry) 0
                           (contains? last-entry :indent-next) (:indent-next last-entry)
                           :else (:indent last-entry))
         actual-indent (or indent fallback-indent)
         actual-message (if (and check-pass (likely-password? value))
                          "* log redacted *"
                          value)
         base-entry (make-entry :value actual-message :level (or level :info) :indent actual-indent)
         entry (cond-> base-entry
                 (some? indent-next) (assoc :indent-next indent-next))
         updated-logs (conj logs entry)]
     (-with-logs sink updated-logs))))

(defn format-log-message
  "Format a log entry for output.

   Args:
   - log-entry: log entry map with :indent, :time, :level, :value keys

   Returns: formatted string"
  [{:keys [indent time level value]}]
  (let [instant (if (instance? Long time)
                  (Instant/ofEpochMilli time)
                  time)
        timestamp (.format fmt/text-date-formatter instant)
        log-level (str/upper-case (name level))
        spaces (str/join (repeat indent " "))]
    (format "%s - %-8s:%s%s" timestamp log-level spaces value)))

(defn- print-log-to-stdout
  "Print a sequence of pre-formatted log lines to stdout."
  [formatted-logs]
  (doseq [formatted formatted-logs]
    (println formatted)))

(defn- print-log-to-file
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
  (let [ts (when timestamp (fmt/filename-timestamp))
        filename (cond
                   (and ts name) (str ts "-" name ".log")
                   ts (str ts ".log")
                   name (str name ".log")
                   :else "logs.log")
        filepath (str dir "/" filename)]
    (spit filepath (str/join "\n" logs))))

(defn print-logs
  "Print a vector of log entries according to output mode.

   Args:
   - logs: vector of log entries
   - out: output mode - :none, :stdout, :file, or :both (default :both)
   - dir: optional output directory for :file or :both modes (default: ./logs)
   - name: optional base filename (without extension) for :file or :both modes
   - timestamp: boolean, whether to prepend timestamp to filename (default: true)

   Output modes:
   - :none   - Don't output logs anywhere
   - :stdout - Print logs to stdout only
   - :file   - Write logs to .log file only
   - :both   - Print to stdout AND write to .log file

   Filename generation:
   - If both name and timestamp: <timestamp>-<n>.log
   - If only timestamp: <timestamp>.log
   - If only name: <n>.log
   - If neither: logs.log

   Each log entry is printed with appropriate formatting."
  [logs & {:keys [out dir name timestamp]
           :or {out :both}}]
  (let [formatted-logs (map format-log-message logs)
        file-opts (cond-> {}
                    dir (assoc :dir dir)
                    name (assoc :name name)
                    (some? timestamp) (assoc :timestamp timestamp))]
    (case out
      :none nil
      :stdout (print-log-to-stdout formatted-logs)
      :file (apply print-log-to-file formatted-logs (apply concat file-opts))
      :both (do
              (print-log-to-stdout formatted-logs)
              (apply print-log-to-file formatted-logs (apply concat file-opts))))))

(defn- print-meta-to-stdout
  "Print bucket metadata to stdout with a header."
  [meta]
  (println "\n=== Bucket Metadata ===")
  (pp/pprint meta))

(defn- meta-filename
  "Generate filename for metadata file based on timestamp and name options.

   Args:
   - name: optional base filename (without extension)
   - timestamp: boolean, whether to prepend timestamp to filename

   Returns: filename string with .edn extension"
  [name timestamp]
  (let [ts (when timestamp (fmt/filename-timestamp))]
    (cond
      (and ts name) (str ts "-" name ".edn")
      ts (str ts ".edn")
      name (str name ".edn")
      :else "meta.edn")))

(defn- print-meta-to-file
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

(defn print-meta
  "Print bucket metadata according to output mode.

   Args:
   - meta: metadata map from bucket
   - out: output mode - :none, :stdout, :file, or :both (default :both)
   - dir: optional output directory for :file or :both modes (default: ./meta)
   - name: optional base filename (without extension) for :file or :both modes
   - timestamp: boolean, whether to prepend timestamp to filename (default: true)

   Output modes:
   - :none   - Don't output metadata anywhere
   - :stdout - Print metadata to stdout only
   - :file   - Write metadata to .edn file only
   - :both   - Print to stdout AND write to .edn file

   Filename generation:
   - If both name and timestamp: <timestamp>-<n>.edn
   - If only timestamp: <timestamp>.edn
   - If only name: <n>.edn
   - If neither: meta.edn"
  [meta & {:keys [out dir name timestamp]
           :or {out :both}}]
  (let [file-opts (cond-> {}
                    dir (assoc :dir dir)
                    name (assoc :name name)
                    (some? timestamp) (assoc :timestamp timestamp))]
    (case out
      :none nil
      :stdout (print-meta-to-stdout meta)
      :file (apply print-meta-to-file meta (apply concat file-opts))
      :both (do
              (print-meta-to-stdout meta)
              (apply print-meta-to-file meta (apply concat file-opts))))))

(defn with-indent
  "Transform a vector of log entries by adding a base indentation.

   Args:
   - logs: vector of log entries
   - additional-indent: indent to add

   Returns: new vector with updated indentation"
  [logs additional-indent]
  (mapv (fn [log-entry]
          (update log-entry :indent + additional-indent))
        logs))

(defn filter-by-level
  "Filter log entries by minimum level.

   Args:
   - logs: vector of log entries
   - min-level: minimum level to include

   Returns: filtered vector of log entries"
  [logs min-level]
  (let [level-order {:debug 0 :info 1 :warning 2 :error 3 :critical 4}
        min-val (level-order min-level 0)]
    (filterv (fn [{:keys [level]}]
               (>= (level-order level 0) min-val))
             logs)))

(defn debug
  "Add a debug log entry.

   Args:
   - logs: existing log vector
   - message: log message string
   - :indent (optional): indentation level

   Examples:
     (debug logs \"Debug message\")
     (debug logs \"Debug message\" :indent 2)"
  [logs message & {:keys [indent]}]
  (if indent
    (log logs message :level :debug :indent indent)
    (log logs message :level :debug)))

(defn info
  "Add an info log entry.

   Args:
   - logs: existing log vector
   - message: log message string
   - :indent (optional): indentation level

   Examples:
     (info logs \"Info message\")
     (info logs \"Info message\" :indent 2)"
  [logs message & {:keys [indent]}]
  (if indent
    (log logs message :level :info :indent indent)
    (log logs message :level :info)))

(defn warning
  "Add a warning log entry.

   Args:
   - logs: existing log vector
   - message: log message string
   - :indent (optional): indentation level

   Examples:
     (warning logs \"Warning message\")
     (warning logs \"Warning message\" :indent 2)"
  [logs message & {:keys [indent]}]
  (if indent
    (log logs message :level :warning :indent indent)
    (log logs message :level :warning)))

(defn error
  "Add an error log entry.

   Args:
   - logs: existing log vector
   - message: log message string
   - :indent (optional): indentation level

   Examples:
     (error logs \"Error message\")
     (error logs \"Error message\" :indent 2)"
  [logs message & {:keys [indent]}]
  (if indent
    (log logs message :level :error :indent indent)
    (log logs message :level :error)))

(defn critical
  "Add a critical log entry.

   Args:
   - logs: existing log vector
   - message: log message string
   - :indent (optional): indentation level

   Examples:
     (critical logs \"Critical message\")
     (critical logs \"Critical message\" :indent 2)"
  [logs message & {:keys [indent]}]
  (if indent
    (log logs message :level :critical :indent indent)
    (log logs message :level :critical)))
