(ns bucket.spouts.helpers.transform
  "Helpers for transforming buckets into serialized forms or summaries."
  (:require [bin.format :as fmt]
            [clojure.java.io :as io]
            [jsonista.core :as json]))

(defn prepare-bucket-for-json
  "Prepare bucket for JSON serialization by converting non-JSON-safe values."
  [bucket]
  (let [{:keys [id name error logs result meta]} bucket
        [exception stacktrace] error]
    {:id (str id)
     :name name
     :error (if exception
              {:exception (str exception)
               :message (ex-message exception)
               :stacktrace stacktrace}
              {:exception nil
               :message stacktrace
               :stacktrace nil})
     :logs (mapv (fn [log]
                   (update log :time #(if (instance? java.time.Instant %)
                                        (str %)
                                        %)))
                 logs)
     :result result
     :meta meta}))

(defn prepare-bucket-for-edn
  "Prepare bucket for EDN serialization by ensuring timestamps are longs."
  [bucket]
  (let [time->long (fn [time]
                     (if (instance? Long time)
                       time
                       (inst-ms time)))]
    (update bucket :logs
            (fn [logs]
              (mapv #(update % :time time->long) logs)))))

(defn serialize-to-edn
  "Serialize bucket to EDN string."
  [bucket]
  (pr-str (prepare-bucket-for-edn bucket)))

(defn serialize-to-json
  "Serialize bucket to JSON string using jsonista."
  [bucket]
  (-> bucket
      prepare-bucket-for-json
      (json/write-value-as-string)))

(defn write-serialized-to-file
  "Write serialized bucket to file."
  [serialized format & {:keys [dir name timestamp?]
                        :or {timestamp? true}}]
  (let [dir-path (or dir ".")
        dir-file (io/file dir-path)]
    (.mkdirs dir-file)
    (let [ts (when timestamp? (fmt/filename-timestamp))
          extension (case format
                      :edn ".edn"
                      :json ".json")
          filename (cond
                     (and ts name) (str ts "-" name extension)
                     ts (str ts extension)
                     name (str name extension)
                     :else (str "bucket" extension))
          filepath (str dir-path "/" filename)]
      (spit filepath serialized)
      (println (str "Bucket serialized to: " filepath))
      filepath)))
