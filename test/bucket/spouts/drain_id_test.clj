(ns bucket.spouts.drain-id-test
  "Tests for bucket spouts drain-id function."
  (:require [bucket :as bucket]
            [bucket.spouts :as spouts]
            [clojure.test :refer [deftest is testing]]))

(deftest drain-id-extracts-id-test
  (testing "extracts id from bucket"
    (let [bucket (bucket/grab "data")
          id (:id bucket)]
      (is (= id (spouts/drain-id bucket))
          "drain-id extracts id from bucket"))))

(deftest drain-id-different-buckets-test
  (testing "different buckets have different ids"
    (let [bucket1 (bucket/grab "data1")
          bucket2 (bucket/grab "data2")
          id1 (spouts/drain-id bucket1)
          id2 (spouts/drain-id bucket2)]
      (is (not= id1 id2)
          "drain-id returns different ids for different buckets"))))

(deftest drain-id-nil-id-test
  (testing "handles bucket with nil id"
    (let [bucket {:result "data" :error [nil nil] :id nil :name "test" :meta {}}]
      (is (nil? (spouts/drain-id bucket))
          "drain-id returns nil when bucket id is nil"))))

(deftest drain-id-custom-id-test
  (testing "extracts custom id from bucket"
    (let [custom-id "custom-bucket-id-123"
          bucket {:result "data" :error [nil nil] :id custom-id :name "test" :meta {}}]
      (is (= custom-id (spouts/drain-id bucket))
          "drain-id extracts custom id from bucket"))))
