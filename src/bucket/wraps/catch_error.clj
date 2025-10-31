(ns bucket.wraps.catch-error
  (:require [bucket :as bucket]
            [bucket.error.entry :as error-entry]))

(defn catch-error
  "Wrap a function to catch exceptions and turn them into response errors.

   Preserves any existing log vector from opts and reuses the wrapped
   function's metadata.

   Args:
   - f: function to wrap (expects opts map with optional :logs vector)

   Returns: wrapped function that delegates to f and converts thrown
            exceptions into {:error [...] :logs ...} maps via bucket/grab."
  [f]
  (let [wrapped (fn [opts]
                  (try
                    (f opts)
                    (catch Exception e
                      (let [err (error-entry/make e)
                            logs (:logs opts [])]
                        (bucket/grab nil :logs logs :error err)))))]
    (with-meta wrapped (meta f))))