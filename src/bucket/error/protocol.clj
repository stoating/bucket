(ns bucket.error.protocol)

(defn ensure-error-vector
  "Normalize an error representation into a two-element vector.

   Accepts:
   - nil                       -> [nil nil]
   - vector of length < 2      -> padded with nil
   - vector of length >= 2     -> truncated to first two entries
   - sequential collections    -> coerced to vector first
   - Throwable instances       -> [throwable nil]
   - anything else             -> [nil nil]"
  [error]
  (cond
    (vector? error)
    (let [[err stacktrace] (concat error (repeat nil))]
      [err stacktrace])

    (nil? error)
    [nil nil]

    (sequential? error)
    (ensure-error-vector (vec error))

    (instance? Throwable error)
    [error nil]

    :else
    [nil nil]))

(defprotocol ErrorSink
  "Protocol for things that can provide and accept error tuples."
  (-current-error [sink] "Return the normalized error vector for the sink.")
  (-with-error [sink error] "Return the sink updated with the provided error vector."))

(extend-protocol ErrorSink
  clojure.lang.IPersistentVector
  (-current-error [sink] (ensure-error-vector sink))
  (-with-error [_ error] (ensure-error-vector error))

  clojure.lang.IPersistentMap
  (-current-error [sink] (ensure-error-vector (:error sink)))
  (-with-error [sink error] (assoc sink :error (ensure-error-vector error)))

  clojure.lang.ISeq
  (-current-error [sink] (ensure-error-vector sink))
  (-with-error [_ error] (ensure-error-vector error))

  nil
  (-current-error [_] [nil nil])
  (-with-error [_ error] (ensure-error-vector error)))
