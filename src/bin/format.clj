(ns bin.format
  "Date and time formatting utilities for filenames and logs."
  (:require [clojure.string :as str])
  (:import [java.time Instant]
           [java.time.format DateTimeFormatter]
           [java.time ZoneId]))

(def date-text
  (-> (DateTimeFormatter/ofPattern "EEE MMM dd HH:mm:ss yyyy")
      (.withZone (ZoneId/systemDefault))))

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
