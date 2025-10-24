(ns bucket.spouts.drain-result-test
  "Tests for bucket spouts drain-result function."
  (:require [bucket :as bucket]
            [bucket.spouts.extract :as spouts]
            [clojure.test :refer [deftest is testing]]))

(deftest drain-result-extracts-result-test
  (testing "extracts complex result from bucket"
    (let [result {:user-id 123 :name "Alice" :roles [:admin :user]}
          bucket (bucket/grab result)]
      (is (= result (spouts/drain-result bucket))
          "drain-result extracts complex result from bucket"))))

(deftest drain-result-nil-result-test
  (testing "handles nil result"
    (let [bucket (bucket/grab nil)]
      (is (nil? (spouts/drain-result bucket))
          "drain-result returns nil when bucket result is nil"))))

(deftest drain-result-string-result-test
  (testing "extracts string result"
    (let [bucket (bucket/grab "hello world")]
      (is (= "hello world" (spouts/drain-result bucket))
          "drain-result extracts string result from bucket"))))

(deftest drain-result-from-error-bucket-test
  (testing "extracts result from error bucket"
    (let [ex (ex-info "Error occurred" {})
          bucket (bucket/grab "partial-result" :error [ex "context"])]
      (is (= "partial-result" (spouts/drain-result bucket))
          "drain-result extracts partial result from error bucket"))))
