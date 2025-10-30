(ns bucket.spouts.helpers.chain
  "Helpers for chaining and combining multiple buckets."
  (:require [bucket :as bucket]))

(defn combine-logs
  "Combine and sort logs from two buckets chronologically."
  [from-bucket to-bucket]
  (->> (concat (:logs from-bucket []) (:logs to-bucket []))
       (sort-by :time)
       vec))

(defn merge-metadata
  "Merge metadata from two buckets according to strategy."
  [from-bucket to-bucket merge-type]
  (case (or merge-type :merge)
    :merge (merge (:meta from-bucket {}) (:meta to-bucket {}))
    :snapshot (let [to-meta (or (:meta to-bucket) {})
                    from-meta (or (:meta from-bucket) {})
                    existing-buckets (:previous-buckets to-meta [])
                    new-bucket {(:id from-bucket) from-meta}
                    updated-buckets (conj existing-buckets new-bucket)]
                (assoc to-meta :previous-buckets updated-buckets))))

(defn combine-values
  "Combine values from two buckets according to pour-type strategy."
  [from-bucket to-bucket pour-type]
  (case (or pour-type :gather)
    :gather (:value (bucket/gather [from-bucket to-bucket]))
    :drop (:value to-bucket)
    :stir-in (:value (bucket/stir-in (:value from-bucket) to-bucket))))
