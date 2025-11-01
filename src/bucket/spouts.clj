(ns bucket.spouts
  "High-level spout helpers intended for end-user consumption."
  (:require [bucket :as bucket]
            [bucket.error :as error]
            [bucket.log :as log]
            [bucket.meta :as meta]
            [bucket.spouts.helpers.chain :as chain]
            [bucket.spouts.helpers.extract :as extract]
            [clj-ulid :as ulid]))

(defn spill
  "Process a bucket: print logs, handle errors, optionally print metadata, dump bucket, return :value.

  Args:
  - bucket: Bucket map
  - log-out: output mode for logs - :none, :stdout, :file, or :both (default :both)
  - meta-out: output mode for metadata - :none, :stdout, :file, or :both (default :both)
  - error-out: output mode for errors - :none, :stdout, :file, or :both (default :both)
  - bucket-out: output mode for the entire bucket - :none, :stdout, :file, or :both (default :none)
  - exit: exit behavior on error - :success, :fail, or :continue (default :fail)
  - out-dir: directory for file outputs (default: current directory)
  - require-value: boolean, when true checks if :value is nil (default false)

  Returns the :value value (may throw if nil and require-value is true)"
  [bucket & {:keys [log-out meta-out error-out bucket-out exit out-dir require-value]
             :or {log-out :both
                  meta-out :both
                  error-out :both
                  bucket-out :both
                  exit :fail
                  require-value false}}]
  (let [name (:name bucket)
        log-formatter (extract/output-formatter log-out log/print {:out log-out :dir out-dir :name name})
        meta-formatter (extract/output-formatter meta-out meta/print {:out meta-out :dir out-dir :name name})
        bucket-formatter (extract/output-formatter bucket-out bucket/print {:out bucket-out :dir out-dir :name name})
        error-formatter (extract/output-formatter error-out error/handle {:out error-out :dir out-dir :name name :exit exit})]
    (extract/spill-formatted bucket
                             :log-formatter log-formatter
                             :meta-formatter meta-formatter
                             :error-formatter error-formatter
                             :bucket-formatter bucket-formatter
                             :require-value require-value)))

(defn pour-into
  "Pour one bucket into another, combining their histories while returning the updated bucket.

   Args:
   - new-bucket: bucket produced by the most recent computation
   - old-bucket: prior bucket whose history should be preserved
   - :new-name (optional) - override the resulting bucket name
   - :meta-merge-type (optional) - metadata merge strategy (:merge or :snapshot, default :merge)
   - :pour-type (optional) - result combination strategy (:gather, :drop (from), :stir-in (from->to))

   Returns: updated bucket containing merged logs, metadata, and value."
  [to-bucket from-bucket & {:keys [new-name meta-merge-type pour-type]
                            :or {meta-merge-type :merge
                                 new-name (:name to-bucket)
                                 pour-type :gather}}]
  {:id (:id to-bucket)
   :name new-name
   :value (chain/combine-values from-bucket to-bucket pour-type)
   :logs (chain/combine-logs from-bucket to-bucket)
   :meta (chain/merge-metadata from-bucket to-bucket meta-merge-type)
   :error (or (:error to-bucket) [nil nil])})

(defn drain-logs
  "Extract just the logs vector from a bucket. Returns the :logs vector."
  [bucket]
  (:logs bucket))

(defn drain-error
  "Extract just the error tuple from a bucket. Returns the :error tuple."
  [bucket]
  (:error bucket))

(defn drain-value
  "Extract just the value from a bucket. Returns the :value value."
  [bucket]
  (:value bucket))

(defn drain-id
  "Extract just the ID from a bucket. Returns the :id value."
  [bucket]
  (:id bucket))

(defn drain-name
  "Extract just the name from a bucket. Returns the :name value."
  [bucket]
  (:name bucket))

(defn drain-timestamp
  "Extract the creation timestamp from a bucket's ULID. Returns milliseconds since epoch."
  [bucket]
  (ulid/ulid->timestamp (:id bucket)))

(defn drain-meta
  "Extract just the metadata map from a bucket. Returns the :meta map."
  [bucket]
  (:meta bucket))