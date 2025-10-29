(ns bin.format
  "Date and time formatting utilities for filenames and logs."
  (:import [java.time Instant]
           [java.time.format DateTimeFormatter]
           [java.time ZoneId]))

;; Date formatter for timestamps in text files
(def text-date
  (-> (DateTimeFormatter/ofPattern "EEE MMM dd HH:mm:ss yyyy")
      (.withZone (ZoneId/systemDefault))))

;; Date formatter for timestamps in filenames
(def filename-date
  (-> (DateTimeFormatter/ofPattern "yyMMddHHmmssSSS")
      (.withZone (ZoneId/systemDefault))))

(defn filename-timestamp
  "Generate a timestamp string for filenames using the standard format."
  []
  (.format filename-date (Instant/now)))
