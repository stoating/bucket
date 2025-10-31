(ns bucket.error
  "Error handling helpers for Bucket processing.

   Error format: [exception-or-nil stacktrace-string-or-nil]"
  (:require [bucket.error.entry :as error-entry]
            [bucket.error.print :as print]
            [bucket.error.protocol :as protocol]))

(defn handle
  "Handle an error tuple by printing/logging details and optionally exiting.

   Args:
   - sink: error vector or bucket map (must contain :error vector)
   - out: output mode - :none, :stdout, :file, or :both (default :both)
   - dir: optional output directory for :file or :both modes (default: ./errors)
   - name: optional base filename (without extension) for :file or :both modes
   - timestamp?: boolean, whether to prepend timestamp to filename (default: true)
   - exit: exit behavior - :success, :fail, or :continue (default :fail)

   Output modes:
   - :none   - Don't output error anywhere
   - :stdout - Print error to stdout only
   - :file   - Write error to .log file only
   - :both   - Print to stdout AND write to .log file

   Filename generation:
   - If both name and timestamp: <timestamp>-<name>.log
   - If only timestamp: <timestamp>.log
   - If only name: <name>.log
   - If neither: error.log

   Exit behavior:
   - :success  - Exit with status 0 if error exists
   - :fail     - Exit with status 1 if error exists (default)
   - :continue - Don't exit, just log/print"
  [sink & {:keys [out dir name timestamp? exit]
           :or {out :both exit :fail}}]
  (let [[err _ :as normalized-error] (protocol/-current-error sink)
        file-opts (cond-> {}
                    dir (assoc :dir dir)
                    name (assoc :name name)
                    (some? timestamp?) (assoc :timestamp? timestamp?))]
    ;; Handle output
    (when err
      (case out
        :none nil
        :stdout (print/->stdout normalized-error)
        :file (apply print/->file normalized-error (apply concat file-opts))
        :both (do
                (print/->stdout normalized-error)
                (apply print/->file normalized-error (apply concat file-opts)))))

    ;; Handle exit
    (when err
      (case exit
        :success (do
                   (println "exit with status success")
                   (System/exit 0))
        :fail (do
                (println "exit with status fail")
                (System/exit 1))
        :continue))))

(defn ?
  "Check if an error tuple represents an actual error.

   Args:
   - sink: error vector or bucket map

   Returns: true if error contains an exception"
  [sink]
  (let [[err _] (protocol/-current-error sink)]
    (some? err)))

(defn wrap-error
  "Wrap a function to catch exceptions and return as error tuple.

   Args:
   - f: function to wrap

   Returns: wrapped function that catches exceptions"
  [f]
  (fn [& args]
    (try
      (apply f args)
      (catch Exception e
        (error-entry/make e)))))

(defn with-context
  "Add context message to an error.

   Args:
   - sink: error vector or bucket map
   - context: string with additional context

   Returns: new error tuple with context added"
  [sink context]
  (let [[err stacktrace] (protocol/-current-error sink)]
    (if err
      (protocol/-with-error sink [(ex-info context {:wrapped err}) stacktrace])
      sink)))
