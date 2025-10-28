(ns bucket.spouts
  "High-level spout helpers intended for end-user consumption."
  (:require [bucket.error :as error]
            [bucket.log :as log]
            [bucket.spouts.chain :as chain]
            [bucket.spouts.extract :as extract]
            [clj-ulid :as ulid]))

;; ----------------------------------------------------------------------------
;; Extract helpers (printing/draining buckets)
;; ----------------------------------------------------------------------------
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
        log-formatter (extract/output-formatter log-out log/print-logs {:out log-out :dir out-dir :name name})
        meta-formatter (extract/output-formatter meta-out log/print-meta {:out meta-out :dir out-dir :name name})
        error-formatter (extract/output-formatter error-out error/handle-error {:out error-out :dir out-dir :name name :exit exit})]
    (extract/spill-formatted bucket
                             :log-formatter log-formatter
                             :meta-formatter meta-formatter
                             :error-formatter error-formatter
                             :require-result require-result)))

(defn drain-logs
  "Extract just the logs vector from a bucket. Returns the :logs vector."
  [bucket]
  (:logs bucket))

(defn drain-error
  "Extract just the error tuple from a bucket. Returns the :error tuple."
  [bucket]
  (:error bucket))

(defn drain-result
  "Extract just the result from a bucket. Returns the :result value."
  [bucket]
  (:result bucket))

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


;; ----------------------------------------------------------------------------
;; Chain helpers (combining buckets)
;; ----------------------------------------------------------------------------
(defn pour-into
  "Pour one bucket into another, combining their histories while returning the updated bucket.

   Args:
   - new-bucket: bucket produced by the most recent computation
   - old-bucket: prior bucket whose history should be preserved
   - :new-name (optional) - override the resulting bucket name
   - :meta-merge-type (optional) - metadata merge strategy (:merge or :snapshot, default :merge)
   - :pour-type (optional) - result combination strategy (:gather, :drop-old, :drop-new,
     :stir-in-old->new, :stir-in-new->old; default :gather)

   Returns: updated bucket containing merged logs, metadata, and result."
  [new-bucket old-bucket & {:keys [new-name meta-merge-type pour-type]
                            :or {meta-merge-type :merge
                                 pour-type :gather}}]
  {:id (:id new-bucket)
   :name (or new-name (:name new-bucket))
   :result (chain/combine-results old-bucket new-bucket pour-type)
   :logs (chain/combine-logs old-bucket new-bucket)
   :meta (chain/merge-metadata old-bucket new-bucket meta-merge-type)
   :error (or (:error new-bucket) [nil nil])})


;; ----------------------------------------------------------------------------
;; Aggregation helpers (combining buckets)
;; ----------------------------------------------------------------------------
(defn collect-metrics
  "Extract timing and performance metrics from bucket logs."
  [bucket]
  (let [logs (:logs bucket)
        times (map :time logs)
        levels (map :level logs)]
    {:total-logs (count logs)
     :log-levels (frequencies levels)
     :first-timestamp (first times)
     :last-timestamp (last times)
     :time-span (when (and (seq times) (> (count times) 1))
                  (- (last times) (first times)))}))

(defn collect-errors
  "Accumulate errors from multiple buckets."
  [buckets]
  (->> buckets
       (map :error)
       (filter (fn [[exception message]] (or exception message)))
       vec))

(defn merge-into
  "Merge multiple buckets' logs into a single destination atom."
  ([buckets logs-ref]
   (merge-into buckets logs-ref 0))
  ([buckets logs-ref base-indent]
   (let [all-logs (mapcat :logs buckets)
         adjusted (mapv (fn [{:keys [indent time level value]}]
                          {:indent (+ indent base-indent)
                           :time time
                           :level level
                           :value value})
                        all-logs)
         results (mapv :result buckets)]
     (swap! logs-ref into adjusted)
     results)))
