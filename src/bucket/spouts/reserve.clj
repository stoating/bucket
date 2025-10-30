(ns bucket.spouts.reserve
  "End-user facing spouts that complement `bucket.spouts` but are typically needed less often."
  (:require [bucket.spouts.helpers.transform :as transform]))

(defn serialize-bucket
  "Serialize and output a bucket in EDN or JSON format.

  Returns the serialized string."
  [bucket & {:keys [format out dir name timestamp?]
             :or {format :edn
                  out :stdout
                  timestamp? true}}]
  (let [serialized (case format
                     :edn (transform/serialize-to-edn bucket)
                     :json (transform/serialize-to-json bucket)
                     (throw (ex-info "Unsupported format" {:format format :supported [:edn :json]})))]
    (case out
      :none nil
      :stdout (println serialized)
      :file (transform/write-serialized-to-file serialized format
                                      :dir dir
                                      :name name
                                      :timestamp? timestamp?)
      :both (do
              (println serialized)
              (transform/write-serialized-to-file serialized format
                                        :dir dir
                                        :name name
                                        :timestamp? timestamp?)))
    serialized))

(defn summarize
  "Create a condensed summary of a bucket, keeping basic identifiers alongside derived stats."
  [bucket]
  (let [{:keys [id name meta error logs value]} bucket
        [exception message] error]
    {:id id
     :name name
     :meta meta
     :log-count (count logs)
     :error-type (cond
                   exception :exception
                   message :message
                   :else nil)
     :value-type (cond
                   (nil? value) :nil
                   (string? value) :string
                   (number? value) :number
                   (boolean? value) :boolean
                   (map? value) :map
                   (vector? value) :vector
                   (seq? value) :seq
                   :else :unknown)}))

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
         values (mapv :value buckets)]
     (swap! logs-ref into adjusted)
     values)))
