(ns bin.format
  "Date and time formatting utilities for filenames and logs."
  (:require [clojure.string :as str])
  (:import [java.time Instant]
           [java.time.format DateTimeFormatter]
           [java.time ZoneId]))

(def date-text
  (-> (DateTimeFormatter/ofPattern "EEE MMM dd HH:mm:ss yyyy")
      (.withZone (ZoneId/systemDefault))))

(defn error-text
  "Format an error tuple as a string for logging.

   Args:
   - error: [exception-or-nil stacktrace-or-nil]

   Returns: formatted error string or nil"
  [error]
  (let [[err stacktrace] error]
    (when err
      (str "Error: " (ex-message err)
           (when stacktrace
             (str "\n" stacktrace))))))

(defn log-text
  "Format a log entry for output.

   Args:
   - log-entry: log entry map with :indent, :time, :level, :value keys

   Returns: formatted string"
  [{:keys [indent time level value]}]
  (let [instant (if (instance? Long time)
                  (Instant/ofEpochMilli time)
                  time)
        timestamp (.format date-text instant)
        log-level (str/upper-case (name level))
        spaces (str/join (repeat indent " "))]
    (format "%s - %-8s:%s%s" timestamp log-level spaces value)))

(def file-date
  (-> (DateTimeFormatter/ofPattern "yyMMddHHmmssSSS")
      (.withZone (ZoneId/systemDefault))))

(defn filename-timestamp
  "Generate a timestamp string for filenames using the standard format."
  []
  (.format file-date (Instant/now)))

(defn filename
  "Generate a filename string using timestamp, name, and formatting options.

   Invocation styles:
   - Positional mode (odd arity): first argument is treated as the `:name`
     and the remaining args must be keyword pairs.
   - Keyword mode   (even arity): all arguments are interpreted as keyword
     pairs, allowing `:name` to be omitted entirely.

   Recognised keyword options:
   - :name        -> base filename without extension
   - :timestamp?  -> prepend timestamp when true (default false)
   - :type        -> fallback base name when no name/timestamp supplied (default \"filename\")
   - :ext         -> file extension without dot (default \"txt\")"
  [& args]
  (let [kw-args (if (odd? (count args))
                  (apply hash-map :name (first args) (rest args))
                  (apply hash-map args))
        {:keys [name timestamp? type ext]
         :or {timestamp? false type "file" ext "txt"}} kw-args
        ts (when timestamp? (filename-timestamp))]
    (cond
      (and ts name) (str ts "-" name "." ext)
      ts (str ts "." ext)
      name (str name "." ext)
      :else (str type "." ext))))
