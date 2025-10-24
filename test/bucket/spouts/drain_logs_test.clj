(ns bucket.spouts.drain-logs-test
  "Tests for bucket spouts drain-logs function."
  (:require [bucket :as bucket]
            [bucket.spouts.extract :as spouts]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(deftest drain-logs-extracts-logs-test
  (testing "extracts logs from bucket"
    (let [logs [{:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Test log"}
                {:indent 1 :time (Instant/parse "2024-01-15T10:30:01Z") :level :debug :value "Debug info"}]
          bucket {:result "data" :error [nil nil] :logs logs}]
      (is (= logs (spouts/drain-logs bucket))
          "drain-logs extracts logs from bucket"))))

(deftest drain-logs-empty-logs-test
  (testing "handles empty logs"
    (let [bucket (bucket/grab "data")]
      (is (= [] (spouts/drain-logs bucket))
          "drain-logs returns empty vector for bucket with no logs"))))

(deftest drain-logs-nil-logs-test
  (testing "handles nil bucket logs"
    (let [bucket {:result "data" :error [nil nil] :logs nil}]
      (is (nil? (spouts/drain-logs bucket))
          "drain-logs returns nil when bucket logs is nil"))))

(deftest drain-logs-complex-logs-test
  (testing "extracts complex log structures"
    (let [logs [{:indent 0 :time 1000 :level :info :value "Start"}
                {:indent 1 :time 1500 :level :debug :value "Processing user 123"}
                {:indent 2 :time 1600 :level :warn :value "Cache miss"}
                {:indent 1 :time 1700 :level :info :value "Completed"}]
          bucket (bucket/grab {:status "success"} :logs logs)]
      (is (= 4 (count (spouts/drain-logs bucket))))
      (is (= :warn (:level (nth (spouts/drain-logs bucket) 2))))
      (is (= "Cache miss" (:value (nth (spouts/drain-logs bucket) 2)))
          "drain-logs extracts complex log structures with all fields"))))
