(ns bucket.spouts.chain
  "Helpers for chaining and combining multiple buckets."
  (:require [bucket :as bucket]))

(defn combine-logs
  "Combine and sort logs from two buckets chronologically."
  [old-bucket new-bucket]
  (->> (concat (:logs old-bucket []) (:logs new-bucket []))
       (sort-by :time)
       vec))

(defn merge-metadata
  "Merge metadata from two buckets according to strategy."
  [old-bucket new-bucket merge-type]
  (case (or merge-type :merge)
    :merge (merge (:meta old-bucket {}) (:meta new-bucket {}))
    :snapshot (let [new-meta (or (:meta new-bucket) {})
                    old-meta (or (:meta old-bucket) {})
                    existing-previous (:previous-buckets new-meta [])
                    new-entry {(:id old-bucket) old-meta}
                    updated-previous (conj existing-previous new-entry)]
                (assoc new-meta :previous-buckets updated-previous))))

(defn combine-results
  "Combine results from two buckets according to pour-type strategy."
  [old-bucket new-bucket pour-type]
  (case (or pour-type :gather)
    :gather (:result (bucket/gather [old-bucket new-bucket]))
    :drop-old (:result new-bucket)
    :drop-new (:result old-bucket)
    :stir-in-old->new (:result (bucket/stir-in (:result old-bucket) new-bucket))
    :stir-in-new->old (:result (bucket/stir-in (:result new-bucket) old-bucket))))
