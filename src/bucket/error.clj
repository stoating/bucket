(ns bucket.error
  "Error handling helpers for Bucket processing.

   Error format: [exception-or-nil stacktrace-string-or-nil]"
  (:require [bin.format :as format]
            [clojure.stacktrace :as st]
            [clojure.java.io :as io]))

(defn make-error
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

(defn- print-error-to-stdout
  "Print error details to stdout."
  [error]
  (let [[err stacktrace] error]
    (when err
      (println (str "error class: " (class err)))
      (println (str "error message: " (ex-message err)))
      (when-let [cause (ex-cause err)]
        (println (str "error cause: " cause)))
      (when stacktrace
        (println "stacktrace:")
        (println stacktrace)))))

(defn- error-filename
  "Generate filename for error log based on timestamp and name options.

   Args:
   - name: optional base filename (without extension)
   - timestamp: boolean, whether to prepend timestamp to filename

   Returns: filename string with .log extension"
  [name timestamp]
  (let [ts (when timestamp (format/filename-timestamp))]
    (cond
      (and ts name) (str ts "-" name ".log")
      ts (str ts ".log")
      name (str name ".log")
      :else "error.log")))

(defn- print-error-to-file
  "Write error details to a file.

   Args:
   - error: [exception-or-nil stacktrace-or-nil]
   - dir: optional output directory (default: ./errors)
   - name: optional base filename (without extension)
   - timestamp: boolean, whether to prepend timestamp to filename (default: true)

   Filename generation:
   - If both name and timestamp: <timestamp>-<n>.log
   - If only timestamp: <timestamp>.log
   - If only name: <n>.log
   - If neither: error.log"
  [error & {:keys [dir name timestamp]
            :or {dir "errors" timestamp true}}]
  (let [dir-file (io/file dir)]
    (.mkdirs dir-file))
  (let [filename (error-filename name timestamp)
        filepath (str dir "/" filename)
        [err stacktrace] error]
    (when err
      (spit filepath
            (str "error class: " (class err) "\n"
                 "error message: " (ex-message err) "\n"
                 (when-let [cause (ex-cause err)]
                   (str "error cause: " cause "\n"))
                 (when stacktrace
                   (str "stacktrace:\n" stacktrace "\n"))))
      (println (str "Error written to: " filepath)))))

(defn handle-error
  "Handle an error tuple by printing/logging details and optionally exiting.

   Args:
   - error: [exception-or-nil stacktrace-or-nil]
   - out: output mode - :none, :stdout, :file, or :both (default :both)
   - dir: optional output directory for :file or :both modes (default: ./errors)
   - name: optional base filename (without extension) for :file or :both modes
   - timestamp: boolean, whether to prepend timestamp to filename (default: true)
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
  [error & {:keys [out dir name timestamp exit]
            :or {out :both exit :fail}}]
  (let [[err _] error
        file-opts (cond-> {}
                    dir (assoc :dir dir)
                    name (assoc :name name)
                    (some? timestamp) (assoc :timestamp timestamp))]
    ;; Handle output
    (when err
      (case out
        :none nil
        :stdout (print-error-to-stdout error)
        :file (apply print-error-to-file error (apply concat file-opts))
        :both (do
                (print-error-to-stdout error)
                (apply print-error-to-file error (apply concat file-opts)))))

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

(defn error?
  "Check if an error tuple represents an actual error.

   Args:
   - error: [exception-or-nil stacktrace-or-nil]

   Returns: true if error contains an exception"
  [error]
  (some? (first error)))

(defn format-error
  "Format an error tuple as a string for logging.

   Args:
   - error: [exception-or-nil stacktrace-or-nil]

   Returns: formatted error string or nil"
  [error]
  (let [[err stacktrace] error]
    (when err
      (str "Error: " (ex-message err)
           (when stacktrace
             (str "\n" stacktrace))))))

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
        (make-error e)))))

(defn with-context
  "Add context message to an error.

   Args:
   - error: [exception-or-nil stacktrace-or-nil]
   - context: string with additional context

   Returns: new error tuple with context added"
  [error context]
  (let [[err stacktrace] error]
    (if err
      [(ex-info context {:wrapped err}) stacktrace]
      error)))
