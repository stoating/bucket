(ns bin.format
  "Date and time formatting utilities for filenames and logs."
  (:import [java.time Instant]
           [java.time.format DateTimeFormatter]
           [java.time ZoneId]))

;; Date formatter for human-readable timestamps in log messages
(def text-date-formatter
  (-> (DateTimeFormatter/ofPattern "EEE MMM dd HH:mm:ss yyyy")
      (.withZone (ZoneId/systemDefault))))

;; Global formatter for timestamps used in filenames (e.g., log/meta/error files)
(def filename-date-formatter
  (-> (DateTimeFormatter/ofPattern "yyMMddHHmmssSSS")
      (.withZone (ZoneId/systemDefault))))

(defn filename-timestamp
  "Generate a timestamp string for filenames using the standard format."
  []
  (.format filename-date-formatter (Instant/now)))
