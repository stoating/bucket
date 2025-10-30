(ns bucket.spouts.helpers.extract
  "Utilities for extracting data from buckets and emitting their contents.")

(defn apply-formatter
  "Apply a formatter to data and print the value if non-nil.

  Args:
  - formatter: function to format the data (can return nil or string)
  - data: the data to format
  - out: output stream/writer

  Returns: nil (side-effecting only)"
  [formatter data out]
  (when formatter
    (when-let [value (formatter data)]
      (binding [*out* out]
        (println value)))))

(defn spill-formatted
  "Spill a bucket using custom formatters for logs, metadata, and errors.

  Args (all keyword arguments):
  - bucket: Bucket map (required as first positional arg)
  - out: output stream/writer (optional, defaults to *out*)
  - log-formatter: function (logs-vector -> nil or string) to handle log entries
  - meta-formatter: function (meta-map -> nil or string) to handle metadata
  - error-formatter: function (error-tuple -> nil or string) to handle error tuples
  - require-value: boolean, when true checks if :value is nil (default false)

  Returns the :value (may throw if nil and require-value is true)"
  [bucket & {:keys [out log-formatter meta-formatter error-formatter require-value]
             :or {out *out*
                  require-value false}}]
  (apply-formatter log-formatter (:logs bucket) out)
  (apply-formatter meta-formatter (:meta bucket) out)
  (apply-formatter error-formatter (:error bucket) out)
  (let [value (:value bucket)]
    (when (and require-value (nil? value))
      (binding [*out* out]
        (println "value is nil. panic. perhaps you want to call spill-formatted with :require-value false")
        (throw (ex-info "value is nil" {:op :spill-formatted :require-value true}))))
    value))

(defn output-formatter
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
