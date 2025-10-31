(ns bucket.wraps.log-args
  (:require [bucket.log :as log]))

(defn log-args
  "Wrap a function to log invocation arguments before execution.

   Captures the arguments (minus the :logs key), performs optional password
   redaction, and ensures the response carries the updated log vector even
   when the wrapped function omits it.

   Args:
   - f: function to wrap (expects opts map containing optional :logs)
   - :check-secrets (optional keyword arg) if true, redact password-like values
     from logged arguments (default true)

   Returns: wrapped function that logs arguments and delegates to f."
  [f & {:keys [check-secrets]
        :or {check-secrets true}}]
  (let [wrapped (fn [opts]
                  (let [logs (:logs opts [])
                        last-entry (peek logs)
                        base-indent (cond
                                      (nil? last-entry) 0
                                      (contains? last-entry :indent-next) (:indent-next last-entry)
                                      :else (:indent last-entry))
                        arguments (dissoc opts :logs)
                        arg-vec (vec arguments)
                        log-entry (fn [current message last?]
                                    (log/log current
                                             message
                                             :indent base-indent
                                             :check-secrets check-secrets
                                             :indent-next (when last? base-indent)))
                        args-logs (if (seq arg-vec)
                                    (let [initial-logs (log-entry logs "args:" false)
                                          last-index (dec (count arg-vec))]
                                      (reduce (fn [acc [idx [k v]]]
                                                (let [message (str "arg: " (pr-str k) ", value: " (pr-str v))
                                                      last? (= idx last-index)]
                                                  (log-entry acc message last?)))
                                              initial-logs
                                              (map-indexed vector arg-vec)))
                                    (log-entry logs "args: {}" true))
                        response (f (assoc opts :logs args-logs))]
                    (if (and (map? response)
                             (nil? (:logs response)))
                      (assoc response :logs args-logs)
                      response)))]
    (with-meta wrapped (meta f))))
