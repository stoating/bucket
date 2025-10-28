(ns bucket.spouts.extract
  "Utilities for extracting data from buckets and emitting their contents."
  (:require [bucket.error :as error]
            [bucket.log :as logging]
            [clj-ulid :as ulid]))

(defn- apply-formatter
  "Apply a formatter to data and print the result if non-nil.

  Args:
  - formatter: function to format the data (can return nil or string)
  - data: the data to format
  - out: output stream/writer

  Returns: nil (side-effecting only)"
  [formatter data out]
  (when formatter
    (when-let [result (formatter data)]
      (binding [*out* out]
        (println result)))))

(defn spill-formatted
  "Spill a bucket using custom formatters for logs, metadata, and errors.

  Args (all keyword arguments):
  - bucket: Bucket map (required as first positional arg)
  - out: output stream/writer (optional, defaults to *out*)
  - log-formatter: function (logs-vector -> nil or string) to handle log entries
  - meta-formatter: function (meta-map -> nil or string) to handle metadata
  - error-formatter: function (error-tuple -> nil or string) to handle error tuples
  - require-result: boolean, when true checks if :result is nil (default false)

  Returns the :result value (may throw if nil and require-result is true)"
  [bucket & {:keys [out log-formatter meta-formatter error-formatter require-result]
             :or {out *out*
                  require-result false}}]
  (apply-formatter log-formatter (:logs bucket) out)
  (apply-formatter meta-formatter (:meta bucket) out)
  (apply-formatter error-formatter (:error bucket) out)
  (let [result (:result bucket)]
    (when (and require-result (nil? result))
      (binding [*out* out]
        (println "result is nil. panic. perhaps you want to call spill-formatted with :require-result false")
        (throw (ex-info "result is nil" {:op :spill-formatted :require-result true}))))
    result))

(defn- output-formatter
  "Create a side-effecting handler based on output mode.

  Args:
  - output-mode: :none, :stdout, :file, or :both
  - output-fn: function to call with data and output options
  - opts: map of options to pass to output-fn (e.g., {:out ... :dir ... :name ...})

  Returns: handler function or nil if output-mode is :none"
  [output-mode output-fn opts]
  (when (not= output-mode :none)
    (fn [data]
      (apply output-fn data (apply concat opts))
      nil)))

(defn spill
  "Process a bucket: print logs, handle errors, optionally print metadata, return :result.

  Args:
  - bucket: Bucket map
  - log-out: output mode for logs - :none, :stdout, :file, or :both (default :both)
  - meta-out: output mode for metadata - :none, :stdout, :file, or :both (default :both)
  - error-out: output mode for errors - :none, :stdout, :file, or :both (default :both)
  - exit: exit behavior on error - :success, :fail, or :continue (default :fail)
  - out-dir: directory for file outputs (default: current directory)
  - require-result: boolean, when true checks if :result is nil (default false)

  Returns the :result value (may throw if nil and require-result is true)"
  [bucket & {:keys [log-out meta-out error-out exit out-dir require-result]
             :or {log-out :both
                  meta-out :both
                  error-out :both
                  exit :fail
                  require-result false}}]
  (let [name (:name bucket)
        log-formatter (output-formatter log-out logging/print-logs {:out log-out :dir out-dir :name name})
        meta-formatter (output-formatter meta-out logging/print-meta {:out meta-out :dir out-dir :name name})
        error-formatter (output-formatter error-out error/handle-error {:out error-out :dir out-dir :name name :exit exit})]
    (spill-formatted bucket
                     :log-formatter log-formatter
                     :meta-formatter meta-formatter
                     :error-formatter error-formatter
                     :require-result require-result)))
(defn drain-logs
  "Extract just the logs vector from a bucket.

  Returns the :logs vector"
  [bucket]
  (:logs bucket))

(defn drain-error
  "Extract just the error tuple from a bucket.

  Returns the :error tuple [exception-or-nil string-or-nil]"
  [bucket]
  (:error bucket))

(defn drain-result
  "Extract just the result from a bucket.

  Returns the :result value"
  [bucket]
  (:result bucket))

(defn drain-id
  "Extract just the ID from a bucket.

  Returns the :id value"
  [bucket]
  (:id bucket))

(defn drain-name
  "Extract just the name from a bucket.

  Returns the :name value"
  [bucket]
  (:name bucket))

(defn drain-timestamp
  "Extract the creation timestamp from a bucket's ULID.

  Returns: Long representing milliseconds since epoch"
  [bucket]
  (ulid/ulid->timestamp (:id bucket)))

(defn drain-meta
  "Extract just the metadata map from a bucket.

  Returns the :meta map"
  [bucket]
  (:meta bucket))
