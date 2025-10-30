(ns bucket.bucketize-test
  "Bucket/bucketize - convert a plain function to operate on Buckets."
  (:require [bucket :as bucket]
            [clojure.string :as s]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(deftest bucketize-creates-bucketized-fns
  (let [bucketized-inc (bucket/bucketize inc)
        bucket-one (bucket/grab 10)
        result (bucketized-inc bucket-one)]
    (is (= {:id (:id bucket-one)
            :name (str (:id bucket-one) "-bucket")
            :meta {}
            :value 10
            :error [nil nil]
            :logs []}
           bucket-one))
    (is (= {:id (:id bucket-one)
            :name (str (:id bucket-one) "-bucket")
            :meta {}
            :value 11
            :error [nil nil]
            :logs []}
           result))))

(deftest bucketize-preserves-logs
  (let [logs [{:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Processing"}]
        bucketized-upper-case (bucket/bucketize s/upper-case)
        bucket-one (bucket/grab "hello" :logs logs)
        result (bucketized-upper-case bucket-one)]
    (is (= {:id (:id bucket-one)
            :name (str (:id bucket-one) "-bucket")
            :meta {}
            :value "hello"
            :error [nil nil]
            :logs logs}
           bucket-one))
    (is (= {:id (:id bucket-one)
            :name (str (:id bucket-one) "-bucket")
            :meta {}
            :value "HELLO"
            :error [nil nil]
            :logs logs}
           result))))

(deftest bucketize-propagates-errors
  (let [times-2 (fn [x] (* x 2))
        bucketized (bucket/bucketize times-2)
        err (bucket/grab :error [(RuntimeException. "boom") "ctx"])
        result (bucketized err)]
    (is (= err
           result))))

(deftest bucketize-metadata-behavior
  (testing "metadata preserved when input bucket has metadata"
    (let [bucketized-inc (bucket/bucketize inc)
          bucket-one (bucket/grab 10 :meta {:source :api :request-id "abc"})
          result (bucketized-inc bucket-one)]
      (is (= {:source :api :request-id "abc"}
             (:meta bucket-one))
          "original bucket should have metadata")
      (is (= {:source :api :request-id "abc"}
             (:meta result))
          "bucketize preserves input bucket metadata")))

  (testing "metadata preserved with logs and complex data"
    (let [logs [{:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Processing"}]
          bucketized-upper-case (bucket/bucketize s/upper-case)
          bucket-one (bucket/grab "data" :logs logs  :meta {:user "charlie" :timestamp 1234567890})
          result (bucketized-upper-case bucket-one)]
      (is (= {:user "charlie" :timestamp 1234567890}
             (:meta result))
          "metadata persists through bucketized operations with logs"))))
