(ns bucket.spouts.drain-meta-test
  "Tests for bucket spouts drain-meta function."
  (:require [bucket :as bucket]
            [bucket.spouts :as spouts]
            [clojure.test :refer [deftest is testing]]))

(deftest drain-meta-extracts-meta-test
  (testing "extracts metadata from bucket"
    (let [meta-data {:user "alice" :timestamp 1234567890}
          bucket {:result "data" :error [nil nil] :id "test-id" :name "test-bucket" :meta meta-data}]
      (is (= meta-data (spouts/drain-meta bucket))
          "drain-meta extracts metadata from bucket"))))

(deftest drain-meta-empty-meta-test
  (testing "handles empty metadata"
    (let [bucket (bucket/grab "data")]
      (is (= {} (spouts/drain-meta bucket))
          "drain-meta returns empty map for bucket with no metadata"))))

(deftest drain-meta-nil-meta-test
  (testing "handles bucket with nil metadata"
    (let [bucket {:result "data" :error [nil nil] :id "test-id" :name "test-bucket" :meta nil}]
      (is (nil? (spouts/drain-meta bucket))
          "drain-meta returns nil when bucket metadata is nil"))))

(deftest drain-meta-complex-meta-test
  (testing "extracts complex metadata structures"
    (let [meta-data {:user {:name "bob" :id 42}
                     :context {:environment "production" :region "us-east"}
                     :tags ["important" "critical"]
                     :version "1.2.3"}
          bucket {:result "data" :error [nil nil] :id "test-id" :name "test-bucket" :meta meta-data}]
      (is (= meta-data (spouts/drain-meta bucket)))
      (is (= "bob" (get-in (spouts/drain-meta bucket) [:user :name])))
      (is (= "production" (get-in (spouts/drain-meta bucket) [:context :environment])))
      (is (= 2 (count (:tags (spouts/drain-meta bucket))))
          "drain-meta extracts complex nested metadata structures"))))
