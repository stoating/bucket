(ns bucket.spouts.drain-name-test
  "Tests for bucket spouts drain-name function."
  (:require [bucket :as bucket]
            [bucket.spouts :as spouts]
            [clojure.test :refer [deftest is testing]]))

(deftest drain-name-extracts-name-test
  (testing "extracts name from bucket"
    (let [bucket (bucket/grab "data")
          name (:name bucket)]
      (is (= name (spouts/drain-name bucket))
          "drain-name extracts name from bucket"))))

(deftest drain-name-default-format-test
  (testing "name follows default format"
    (let [bucket (bucket/grab "data")
          id (:id bucket)
          expected-name (str id "-bucket")]
      (is (= expected-name (spouts/drain-name bucket))
          "drain-name returns name in id-bucket format by default"))))

(deftest drain-name-nil-name-test
  (testing "handles bucket with nil name"
    (let [bucket {:result "data" :error [nil nil] :id "test-id" :name nil :meta {}}]
      (is (nil? (spouts/drain-name bucket))
          "drain-name returns nil when bucket name is nil"))))

(deftest drain-name-custom-name-test
  (testing "extracts custom name from bucket"
    (let [custom-name "my-special-bucket"
          bucket {:result "data" :error [nil nil] :id "test-id" :name custom-name :meta {}}]
      (is (= custom-name (spouts/drain-name bucket))
          "drain-name extracts custom name from bucket"))))
