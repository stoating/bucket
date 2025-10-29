(ns bucket.log
  "Logging helpers for building and printing log entries.

   Log entry format: {:indent int :time inst :level keyword :value string}
   - :indent: integer indentation level
   - :time: java.time.Instant timestamp
   - :level: one of :debug :info :warning :error :critical
   - :value: string message content"
  (:refer-clojure :exclude [filter])
  (:require [bin.format :as format]
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

(defn- ensure-log-vector
  "Normalize a log collection into a vector."
  [logs]
  (cond
    (vector? logs) logs
    (nil? logs) []
    (sequential? logs) (vec logs)
    :else []))

(defprotocol LogSink
  "Protocol for things that can accumulate log entries."
  (-current-logs [sink] "Return the current log vector for the sink.")
  (-with-logs [sink logs] "Return the sink updated with the provided logs vector."))

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

(defn- ->log-opts
  "Convert mixed positional/keyword arguments into a unified options map."
  [a b rest]
  (if (odd? (+ 2 (count rest)))
    (apply hash-map :value a b rest)
    (apply hash-map a b rest)))

(defn- append-log-entry
  "Internal shared implementation for appending a log entry to a sink."
  [sink {:keys [value level indent check-pass indent-next]}]
  (let [logs (-current-logs sink)
        last-entry (peek logs)
        fallback-indent (cond
                          (nil? last-entry) 0
                          (contains? last-entry :indent-next) (:indent-next last-entry)
                          :else (:indent last-entry))
        actual-indent (or indent fallback-indent)
        actual-message (if (and check-pass (likely-password? value))
                         "* log redacted *"
                         value)
        base-entry (make-entry :value actual-message
                               :level (or level :info)
                               :indent actual-indent)
        entry (cond-> base-entry
                (some? indent-next) (assoc :indent-next indent-next))
        updated-logs (conj logs entry)]
    (-with-logs sink updated-logs)))

(defn format-log-message
  "Format a log entry for output.

   Args:
   - log-entry: log entry map with :indent, :time, :level, :value keys

   Returns: formatted string"
  [{:keys [indent time level value]}]
  (let [instant (if (instance? Long time)
                  (Instant/ofEpochMilli time)
                  time)
        timestamp (.format format/text-date instant)
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
  (let [ts (when timestamp (format/filename-timestamp))
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
  (let [ts (when timestamp (format/filename-timestamp))]
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

(defn- ->millis
  "Normalize an instant or millisecond timestamp into a long value."
  [t]
  (cond
    (instance? Instant t) (inst-ms t)
    (number? t) (long t)
    :else nil))

(def ^:private level-order
  {:debug 0 :info 1 :warning 2 :error 3 :critical 4})

(defn- filter-level
  [logs type value]
  (let [target (level-order value 0)]
    (filterv (fn [{:keys [level]}]
               (let [lvl (level-order level 0)]
                 (case type
                   :lte (<= lvl target)
                   :gte (>= lvl target)
                   :eq (= lvl target)
                   false)))
             logs)))

(defn- filter-indent
  [logs type value]
  (let [target (if (number? value) value 0)]
    (filterv (fn [{:keys [indent]}]
               (let [indent (or indent 0)]
                 (case type
                   :lte (<= indent target)
                   :gte (>= indent target)
                   :eq (= indent target)
                   false)))
             logs)))

(defn- filter-time
  [logs type value]
  (let [comparison-ms (or (->millis value)
                          (inst-ms (Instant/now)))]
    (filterv (fn [{:keys [time]}]
               (let [entry-ms (->millis time)]
                 (when entry-ms
                   (case type
                     :lte (<= entry-ms comparison-ms)
                     :gte (>= entry-ms comparison-ms)
                     :eq (= entry-ms comparison-ms)
                     false))))
             logs)))

(defn- filter-value
  [logs type value]
  (let [pattern (if (instance? java.util.regex.Pattern value)
                  value
                  (re-pattern (str value)))]
    (filterv (fn [{:keys [value]}]
               (let [matches? (some? (and value (re-find pattern value)))]
                 (case type
                   :eq matches?
                   :neq (not matches?)
                   false)))
             logs)))

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
        logs (-current-logs sink)
        filtered (case mode
                   :level (filter-level logs type value)
                   :indent (filter-indent logs type value)
                   :time (filter-time logs type value)
                   :value (filter-value logs type value)
                   logs)]
    (-with-logs sink filtered)))

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
   (append-log-entry sink {:value message}))
  ([sink a b & rest]
   (let [opts (->log-opts a b rest)]
     (append-log-entry sink opts))))

(defn- log-with-level
  "Internal helper used by level-specific logging functions.

   Mirrors the `log` calling conventions while forcing the :level key."
  ([level sink message]
   (append-log-entry sink {:value message :level level}))
  ([level sink a b & rest]
   (let [opts (->log-opts a b rest)
         opts-with-level (assoc opts :level level)]
     (append-log-entry sink opts-with-level))))

(defn debug
  "Add a debug log entry to a vector or bucket sink.

   Accepts the same keyword options as `log`, including :indent, :check-pass,
   and :indent-next."
  ([sink message]
   (log-with-level :debug sink message))
  ([sink a b & rest]
   (apply log-with-level :debug sink a b rest)))

(defn info
  "Add an info log entry to a vector or bucket sink.

   Accepts the same keyword options as `log`, including :indent, :check-pass,
   and :indent-next."
  ([sink message]
   (log-with-level :info sink message))
  ([sink a b & rest]
   (apply log-with-level :info sink a b rest)))

(defn warning
  "Add a warning log entry to a vector or bucket sink.

   Accepts the same keyword options as `log`, including :indent, :check-pass,
   and :indent-next."
  ([sink message]
   (log-with-level :warning sink message))
  ([sink a b & rest]
   (apply log-with-level :warning sink a b rest)))

(defn error
  "Add an error log entry to a vector or bucket sink.

   Accepts the same keyword options as `log`, including :indent, :check-pass,
   and :indent-next."
  ([sink message]
   (log-with-level :error sink message))
  ([sink a b & rest]
   (apply log-with-level :error sink a b rest)))

(defn critical
  "Add a critical log entry to a vector or bucket sink.

   Accepts the same keyword options as `log`, including :indent, :check-pass,
   and :indent-next."
  ([sink message]
   (log-with-level :critical sink message))
  ([sink a b & rest]
   (apply log-with-level :critical sink a b rest)))
