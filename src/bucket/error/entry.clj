(ns bucket.error.entry
  (:require [clojure.stacktrace :as st]))

(defn make
  "Create an error tuple from an exception.

   Args:
   - e: Exception or nil
   - stacktrace: optional stacktrace string (will be generated if not provided)

   Returns: [exception stacktrace-string]"
  ([e]
   (if e
     [e (with-out-str (st/print-stack-trace e))]
     [nil nil]))
  ([e stacktrace]
   [e stacktrace]))