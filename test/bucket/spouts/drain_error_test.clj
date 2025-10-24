(ns bucket.spouts.drain-error-test
  "Tests for bucket spouts drain-error function."
  (:require [clojure.test :refer [deftest is testing]]
            [bucket :as bucket]
            [bucket.spouts.extract :as spouts]))

(deftest drain-error-extracts-error-tuple-test
  (testing "extracts error tuple from bucket with exception"
    (let [ex (ex-info "Test error" {:code 500})
          bucket (bucket/grab :error [ex "context"])]
      (is (= [ex "context"] (spouts/drain-error bucket))
          "drain-error extracts error tuple with exception and context"))))

(deftest drain-error-no-error-test
  (testing "handles bucket with no error"
    (let [bucket (bucket/grab "data")]
      (is (= [nil nil] (spouts/drain-error bucket))
          "drain-error returns [nil nil] for bucket with no error"))))

(deftest drain-error-message-only-test
  (testing "extracts message-only error"
    (let [bucket (bucket/grab :error [nil "error message"])]
      (is (= [nil "error message"] (spouts/drain-error bucket))
          "drain-error extracts error message without exception"))))

(deftest drain-error-exception-only-test
  (testing "extracts exception-only error"
    (let [ex (RuntimeException. "Runtime failure")
          bucket (bucket/grab :error [ex nil])]
      (is (= [ex nil] (spouts/drain-error bucket)))
      (is (instance? RuntimeException (first (spouts/drain-error bucket))))
      (is (= "Runtime failure" (.getMessage (first (spouts/drain-error bucket))))
          "drain-error extracts exception without context message"))))
