(ns bucket.spouts.merge-into-test
  "Tests for bucket spouts merge-into function."
  (:require [bucket :as bucket]
            [bucket.spouts.aggregate :as spouts]
            [clojure.test :refer [deftest is testing]]))

(deftest merge-into-multiple-buckets-test
  (testing "merges logs from multiple buckets"
    (let [logs1 [{:indent 0 :time 1000 :level :info :value "Bucket1 log1"}
                 {:indent 1 :time 1001 :level :debug :value "Bucket1 log2"}]
          logs2 [{:indent 0 :time 2000 :level :info :value "Bucket2 log1"}]
          bucket1 (bucket/grab "result1" :logs logs1)
          bucket2 (bucket/grab "result2" :logs logs2)
          logs-ref (atom [])
          results (spouts/merge-into [bucket1 bucket2] logs-ref)]
      (is (= ["result1" "result2"] results))
      (is (= 3 (count @logs-ref)))
      (is (= "Bucket1 log1" (:value (first @logs-ref))))
      (is (= "Bucket2 log1" (:value (last @logs-ref)))
          "merge-into appends logs from multiple buckets into shared atom and returns results"))))

(deftest merge-into-with-base-indent-test
  (testing "adjusts indentation with base indent"
    (let [logs [{:indent 1 :time 1000 :level :info :value "Nested log"}]
          bucket (bucket/grab "result" :logs logs)
          logs-ref (atom [])
          results (spouts/merge-into [bucket] logs-ref 2)]
      (is (= ["result"] results))
      (is (= 1 (count @logs-ref)))
      (is (= 3 (:indent (first @logs-ref)))
          "merge-into adds base indent to all log entries"))))

(deftest merge-into-empty-sequence-test
  (testing "handles empty bucket sequence"
    (let [logs-ref (atom [])
          results (spouts/merge-into [] logs-ref)]
      (is (= [] results))
      (is (= [] @logs-ref)
          "merge-into returns empty results for empty bucket sequence"))))

(deftest merge-into-append-to-existing-test
  (testing "appends to existing logs"
    (let [existing-log {:indent 0 :time 999 :level :info :value "Existing"}
          logs-ref (atom [existing-log])
          new-log {:indent 0 :time 1000 :level :info :value "New"}
          bucket (bucket/grab "result" :logs [new-log])
          results (spouts/merge-into [bucket] logs-ref)]
      (is (= ["result"] results))
      (is (= 2 (count @logs-ref)))
      (is (= "Existing" (:value (first @logs-ref))))
      (is (= "New" (:value (second @logs-ref)))
          "merge-into appends new logs to existing logs in atom"))))
