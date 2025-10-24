(ns bucket.spouts.transform
  "Helpers for transforming buckets into serialized forms or summaries."
  (:require [bin.format :as fmt]
            [clojure.java.io :as io]
            [jsonista.core :as json]))

(defn- prepare-bucket-for-json
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

(defn- prepare-bucket-for-edn
  "Prepare bucket for EDN serialization by ensuring timestamps are longs."
  [bucket]
  (let [time->long (fn [time]
                     (if (instance? Long time)
                       time
                       (inst-ms time)))]
    (update bucket :logs
            (fn [logs]
              (mapv #(update % :time time->long) logs)))))

(defn- serialize-to-edn
  "Serialize bucket to EDN string."
  [bucket]
  (pr-str (prepare-bucket-for-edn bucket)))

(defn- serialize-to-json
  "Serialize bucket to JSON string using jsonista."
  [bucket]
  (-> bucket
      prepare-bucket-for-json
      (json/write-value-as-string)))

(defn- write-serialized-to-file
  "Write serialized bucket to file."
  [serialized format & {:keys [dir name timestamp]
                        :or {timestamp true}}]
  (let [dir-path (or dir ".")
        dir-file (io/file dir-path)]
    (.mkdirs dir-file)
    (let [ts (when timestamp (fmt/filename-timestamp))
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

(defn serialize-bucket
  "Serialize and output a bucket in EDN or JSON format.

  Returns the serialized string."
  [bucket & {:keys [format out dir name timestamp]
             :or {format :edn
                  out :stdout
                  timestamp true}}]
  (let [serialized (case format
                     :edn (serialize-to-edn bucket)
                     :json (serialize-to-json bucket)
                     (throw (ex-info "Unsupported format" {:format format :supported [:edn :json]})))]
    (case out
      :none nil
      :stdout (println serialized)
      :file (write-serialized-to-file serialized format
                                      :dir dir
                                      :name name
                                      :timestamp timestamp)
      :both (do
              (println serialized)
              (write-serialized-to-file serialized format
                                        :dir dir
                                        :name name
                                        :timestamp timestamp)))
    serialized))

(defn summarize
  "Create a condensed summary of a bucket, keeping basic identifiers alongside derived stats."
  [bucket]
  (let [{:keys [id name meta error logs result]} bucket
        [exception message] error]
    {:id id
     :name name
     :meta meta
     :log-count (count logs)
     :has-error (boolean (or exception message))
     :error-type (cond
                   exception :exception
                   message :message
                   :else nil)
     :result-type (cond
                    (nil? result) :nil
                    (string? result) :string
                    (number? result) :number
                    (boolean? result) :boolean
                    (map? result) :map
                    (vector? result) :vector
                    (seq? result) :seq
                    :else :unknown)
     :result-nil? (nil? result)}))
