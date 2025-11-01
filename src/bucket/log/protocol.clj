(ns bucket.log.protocol)

(defn ensure-log-vector
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

  clojure.lang.ISeq
  (-current-logs [sink] (ensure-log-vector sink))
  (-with-logs [_ logs] logs)

  nil
  (-current-logs [_] [])
  (-with-logs [_ logs] logs))