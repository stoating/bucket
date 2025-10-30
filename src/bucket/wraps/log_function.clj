(ns bucket.wraps.log-function
  (:require [bin.defaults :as default]
            [bucket.log :as log]))

(defn log-function
  "Wrap a function to add entry/exit logging using indentation.

   Emits \"-->\"/\"<--\" trace lines around the wrapped invocation, uses the
   provided spacing to nest indentation, and resets indentation for subsequent
   log entries via :indent-next metadata.

   Args:
   - f: function to wrap (should take opts map with :logs key)
   - :spacing (optional keyword arg) controls indentation width (default 4)
   - :log-args? (optional keyword arg) when true delegates to log-args
     (default false)
   - :args-check-secrets (optional keyword arg) controls password checking when
     logging args (default true)

   Returns: wrapped function with entry/exit logging."
  [f & {:keys [spacing]
        :or {spacing default/default-spacing}}]
  (let [wrapped (fn [opts]
                  (let [logs (:logs opts [])
                        base-indent (if (empty? logs) 0 (:indent (last logs)))
                        indent (+ base-indent spacing)
                        fname (or (some-> f meta :name str)
                                  "")

                        entry-logs (log/log logs
                                                (str "--> " fname)
                                                :level :info
                                                :indent indent
                                                :indent-next indent)
                        response (f (assoc opts :logs entry-logs))
                        response-logs (or (:logs response) entry-logs)
                        final-logs (log/log response-logs
                                                (str "<-- " fname)
                                                :level :info
                                                :indent indent
                                                :indent-next base-indent)]
                    (assoc response :logs final-logs)))]
    (with-meta wrapped (meta f))))
