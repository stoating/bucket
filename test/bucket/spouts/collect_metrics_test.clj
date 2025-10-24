(ns bucket.spouts.collect-metrics-test
  "Tests for bucket spouts collect-metrics function."
  (:require [bucket :as bucket]
            [bucket.spouts.aggregate :as spouts]
            [clojure.test :refer [deftest is testing]]))

(deftest collect-metrics-with-logs-test
  (testing "extracts metrics from bucket with logs"
    (let [logs [{:indent 0 :time 1000 :level :info :value "Start"}
                {:indent 1 :time 1500 :level :debug :value "Processing"}
                {:indent 0 :time 2000 :level :error :value "Failed"}]
          bucket {:result "data" :error [nil nil] :logs logs}
          metrics (spouts/collect-metrics bucket)]
      (is (= {:total-logs 3
              :log-levels {:info 1 :debug 1 :error 1}
              :first-timestamp 1000
              :last-timestamp 2000
              :time-span 1000}
             metrics)
          "collect-metrics calculates total logs, level counts, and time span"))))

(deftest collect-metrics-empty-logs-test
  (testing "handles empty logs"
    (let [bucket (bucket/grab "data")
          metrics (spouts/collect-metrics bucket)]
      (is (= {:total-logs 0
              :log-levels {}
              :first-timestamp nil
              :last-timestamp nil
              :time-span nil}
             metrics)
          "collect-metrics returns zero counts and nil timestamps for bucket with no logs"))))

(deftest collect-metrics-single-log-test
  (testing "handles single log entry"
    (let [logs [{:indent 0 :time 1000 :level :info :value "Only log"}]
          bucket {:result "data" :error [nil nil] :logs logs}
          metrics (spouts/collect-metrics bucket)]
      (is (= {:total-logs 1
              :log-levels {:info 1}
              :first-timestamp 1000
              :last-timestamp 1000
              :time-span nil}
             metrics)
          "collect-metrics returns nil time-span for single log entry"))))

(deftest collect-metrics-mixed-levels-test
  (testing "correctly counts different log levels"
    (let [logs [{:indent 0 :time 1000 :level :info :value "Info 1"}
                {:indent 0 :time 1100 :level :info :value "Info 2"}
                {:indent 0 :time 1200 :level :warn :value "Warning"}
                {:indent 0 :time 1300 :level :error :value "Error"}
                {:indent 0 :time 1400 :level :debug :value "Debug"}]
          bucket (bucket/grab "result" :logs logs)
          metrics (spouts/collect-metrics bucket)]
      (is (= 5 (:total-logs metrics)))
      (is (= {:info 2 :warn 1 :error 1 :debug 1} (:log-levels metrics)))
      (is (= 400 (:time-span metrics))
          "collect-metrics aggregates counts by level and calculates time span across multiple log levels"))))
