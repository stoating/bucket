(ns bucket.spouts.aggregate
  "Aggregation helpers for buckets.")

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
