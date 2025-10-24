(ns bucket.spouts.collect-errors-test
  "Tests for bucket spouts collect-errors function."
  (:require [bucket :as bucket]
            [bucket.spouts.aggregate :as spouts]
            [clojure.test :refer [deftest is testing]]))

(deftest collect-errors-from-multiple-buckets-test
  (testing "collects errors from multiple buckets"
    (let [ex1 (ex-info "Error 1" {})
          ex2 (ex-info "Error 2" {})
          buckets [(bucket/grab "success")
                   (bucket/grab :error [ex1 "context1"])
                   (bucket/grab :error [nil "message-only"])
                   (bucket/grab "another-success")
                   (bucket/grab :error [ex2 nil])]
          errors (spouts/collect-errors buckets)]
      (is (= 3 (count errors)))
      (is (= [ex1 "context1"] (first errors)))
      (is (= [nil "message-only"] (second errors)))
      (is (= [ex2 nil] (nth errors 2))
          "collect-errors extracts error tuples from failed buckets while ignoring successful ones"))))

(deftest collect-errors-no-errors-test
  (testing "handles buckets with no errors"
    (let [buckets [(bucket/grab "success1")
                   (bucket/grab "success2")
                   (bucket/grab {:status "ok"})]
          errors (spouts/collect-errors buckets)]
      (is (= [] errors)
          "collect-errors returns empty list when all buckets are successful"))))

(deftest collect-errors-empty-sequence-test
  (testing "handles empty bucket sequence"
    (let [errors (spouts/collect-errors [])]
      (is (= [] errors)
          "collect-errors returns empty list when given empty bucket sequence"))))

(deftest collect-errors-mixed-error-types-test
  (testing "collects different types of errors"
    (let [runtime-ex (RuntimeException. "Runtime error")
          illegal-ex (IllegalArgumentException. "Bad argument")
          buckets [(bucket/grab :error [runtime-ex "runtime context"])
                   (bucket/grab "success")
                   (bucket/grab :error [nil "validation failed"])
                   (bucket/grab :error [illegal-ex nil])]
          errors (spouts/collect-errors buckets)]
      (is (= 3 (count errors)))
      (is (instance? RuntimeException (first (first errors))))
      (is (= "validation failed" (second (second errors))))
      (is (instance? IllegalArgumentException (first (nth errors 2)))
          "collect-errors handles different exception types and message-only errors"))))
