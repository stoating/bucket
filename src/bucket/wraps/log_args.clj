(ns bucket.wraps.log-args
  (:require [bucket.log :as log]))

(defn log-args
  "Wrap a function to log invocation arguments before execution.

   Captures the arguments (minus the :logs key), performs optional password
   redaction, and ensures the response carries the updated log vector even
   when the wrapped function omits it.

   Args:
   - f: function to wrap (expects opts map containing optional :logs)
   - :check-pass (optional keyword arg) if true, redact password-like values
     from logged arguments (default true)

   Returns: wrapped function that logs arguments and delegates to f."
  [f & {:keys [check-pass]
        :or {check-pass true}}]
  (let [wrapped (fn [opts]
                  (let [logs (:logs opts [])
                        last-entry (peek logs)
                        base-indent (cond
                                      (nil? last-entry) 0
                                      (contains? last-entry :indent-next) (:indent-next last-entry)
                                      :else (:indent last-entry))
                        args-text (str "args: " (pr-str (dissoc opts :logs)))
                        args-logs (log/log logs
                                           args-text
                                           :indent base-indent
                                           :check-pass check-pass
                                           :indent-next base-indent)
                        response (f (assoc opts :logs args-logs))]
                    (if (and (map? response)
                             (nil? (:logs response)))
                      (assoc response :logs args-logs)
                      response)))]
    (with-meta wrapped (meta f))))
