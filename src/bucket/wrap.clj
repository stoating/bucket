(ns bucket.wrap
  "Composable wrappers/decorators for action functions.

   - Convert thrown exceptions into Bucket-style response maps
   - Log invocation arguments with optional sensitive redaction
   - Emit entry/exit traces with controllable indentation
   - Capture stdout output and merge it back into structured logs"
  (:require [bucket :as bucket]
            [bucket.wraps.compose :as compose]))

(defn wrap
  "Composes wrappers and allows the caller to request bucketization of plain functions
   and optionally preseed Bucket metadata.

   Forwarded options (same defaults and behavior as `wrap`):
   - :catch-error?       boolean (default true)  — enable error-as-value wrapping
   - :log-args?          boolean (default true)  — log sanitized arguments before invocation
   - :args-check-secrets boolean (default true)  — when logging args, redact password-like values
   - :log-function?      boolean (default true)  — enable entry/exit tracing
   - :spacing            integer  (default 4)     — indentation spacing used by log-function
   - :redirect-stdout?   boolean (default true)  — capture stdout output into logs
   - :stack-exclude      map     (default {})    — forwarded to redirect-stdout; supports
       :exclude vector/seq of namespace prefixes to omit from depth calculations
       :mode    keyword (:append default, or :replace) to control how exclusions apply
   - :redirect-mode      keyword (default :basic) — redirect-stdout strategy; either
       :basic       capture lines verbatim using caller indentation
       :depth-aware capture output with stack-depth-aware indentation

   Additional options handled by `wrap`:
   - :bucketize boolean (default false) — when true, `f` is passed through
     `bucket/bucketize` before delegating to `wrap`.
   - :bucket    map     (default {})    — optional partially-populated bucket returned by
     `bucket/grab` that can provide initial `:logs` (vector), `:error` (pair), `:meta`
     (map), and/or `:name` (keyword/string). When supplied with `:value`, the wrapped
     function runs immediately with that value instead of waiting for external input.

   Examples:
   ```clojure
   (wrap work-fn)                        ; identical to (wrap work-fn)
   (wrap inc {:bucketize true})          ; bucketize plain inc before wrapping
   (wrap inc {:bucketize true
              :log-args? false
              :log-function? false})
  ```"
  ([f]
   (wrap f nil {}))
  ([f opts]
   (if (map? opts)
     (wrap f nil opts)
     (wrap f opts {})))
  ([f input {:keys [bucketize bucket]
             :or {bucketize false
                  bucket {}}
             :as opts}]
   (let [wrap-opts (dissoc opts :bucketize :bucket)
         f* (if bucketize
              (bucket/bucketize f)
              f)
         wrapped (compose/wrappers f* wrap-opts)
         bucket-map (or bucket {})
         bucket-provided? (contains? opts :bucket)
         bucket-has-value? (contains? bucket-map :value)
         base-args (into [:logs (or (:logs bucket-map) [])
                          :error (or (:error bucket-map) [nil nil])
                          :meta (or (:meta bucket-map) {})]
                         (when (contains? bucket-map :name)
                           [:name (:name bucket-map)]))]
     (cond
       (or (some? input) bucket-has-value?)
       (let [value (if bucket-has-value?
                     (:value bucket-map)
                     input)]
         (wrapped (apply bucket/grab (into [:value value] base-args))))

       bucket-provided?
       (apply bucket/grab (into [:value wrapped] base-args))

       :else
       wrapped))))
