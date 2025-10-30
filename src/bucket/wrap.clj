(ns bucket.wrap
  "Composable wrappers/decorators for action functions.

   - Convert thrown exceptions into Bucket-style response maps
   - Log invocation arguments with optional sensitive redaction
   - Emit entry/exit traces with controllable indentation
   - Capture stdout output and merge it back into structured logs"
  (:require [bin.defaults :as default]
            [bucket.wraps.redirect-stdout :refer [redirect-stdout]]
            [bucket.wraps.catch-error :refer [catch-error]]
            [bucket.wraps.log-function :refer [log-function]]
            [bucket.wraps.log-args :refer [log-args]]
            [clojure.math :as math]))

(defn wrap
  "Main decorator that combines error handling and function logging.

   Args:
   - f: function to wrap (should take opts map and return Bucket)
   - opts: optional map with keys:
     - :catch-error?       enable error-as-value wrapping (default true)
     - :log-args?          log sanitized arguments before invocation (default true)
     - :args-check-secrets  when logging args, redact password-like values (default true)
     - :log-function?      enable entry/exit tracing (default true)
     - :spacing            indentation spacing used by log-function (default 4)
     - :redirect-stdout?   capture stdout output into logs (default true)
     - :stack-exclude      map forwarded to redirect-stdout to control stack filtering

   Returns the decorated function."
  ([f]
   (wrap f {}))
  ([f {:keys [catch-error? log-args? args-check-secrets log-function? spacing
              redirect-stdout? stack-exclude redirect-mode]
       :or {args-check-secrets true
            catch-error? true
            log-args? true
            log-function? true
            redirect-stdout? true
            redirect-mode :basic
            spacing default/spacing
            stack-exclude {}}}]
   (cond-> f
     catch-error? catch-error
     redirect-stdout? (redirect-stdout
                       :spacing (int (math/ceil (/ spacing 2.0)))
                       :stack-exclude stack-exclude
                       :mode redirect-mode)
     log-args? (log-args :check-secrets args-check-secrets)
     log-function? (log-function :spacing spacing))))
