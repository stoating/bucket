(ns bucket.spouts.drain-timestamp-test
  "Tests for bucket spouts drain-timestamp function."
  (:require [bucket :as bucket]
            [bucket.spouts :as spouts]
            [clojure.test :refer [deftest is testing]]))

(deftest drain-timestamp-extracts-timestamp-test
  (testing "extracts timestamp from bucket ULID"
    (let [before (.toEpochMilli (java.time.Instant/now))
          bucket (bucket/grab "data")
          timestamp (spouts/drain-timestamp bucket)
          after (.toEpochMilli (java.time.Instant/now))]
      (is (int? timestamp))
      (is (>= timestamp before))
      (is (<= timestamp after)
          "drain-timestamp extracts timestamp from bucket ULID within expected time range"))))

(deftest drain-timestamp-different-buckets-test
  (testing "different buckets created at different times have different timestamps"
    (let [bucket1 (bucket/grab "data1")
          ts1 (spouts/drain-timestamp bucket1)]
      (Thread/sleep 10)
      (let [bucket2 (bucket/grab "data2")
            ts2 (spouts/drain-timestamp bucket2)]
        (is (not= ts1 ts2))
        (is (< ts1 ts2)
            "timestamps from buckets created at different times are different and ordered")))))

(deftest drain-timestamp-ulid-format-test
  (testing "timestamp can be extracted from ULID format"
    (let [bucket (bucket/grab "test-data")
          ulid-id (:id bucket)
          timestamp (spouts/drain-timestamp bucket)]
      (is (string? ulid-id))
      (is (= 26 (count ulid-id)))
      (is (int? timestamp))
      (let [now (.toEpochMilli (java.time.Instant/now))
            diff-ms (- now timestamp)]
        (is (< diff-ms 1000)
            "drain-timestamp extracts valid timestamp from 26-character ULID format")))))
