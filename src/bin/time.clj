(ns bin.time
  (:import [java.time Instant]))

(defn ->millis
  "Normalize an instant or millisecond timestamp into a long value."
  [t]
  (cond
    (instance? Instant t) (inst-ms t)
    (number? t) (long t)
    :else nil))