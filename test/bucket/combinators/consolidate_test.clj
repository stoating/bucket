(ns bucket.combinators.consolidate-test
  "Bucket/consolidate - flatten nested Buckets."
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(deftest consolidate-flattens-nested-bucket
  (testing "consolidate extracts inner bucket result"
    (let [bucket-one (bucket/grab 42)
          bucket-two (bucket/grab bucket-one)
          consolidated (bucket/consolidate bucket-two)]
      (is (= {:id (:id bucket-one)
              :name (str (:id bucket-one) "-bucket")
              :meta {}
              :result 42
              :error [nil nil]
              :logs []}
             bucket-one))
      (is (= {:id (:id bucket-two)
              :name (str (:id bucket-two) "-bucket")
              :meta {}
              :result bucket-one
              :error [nil nil]
              :logs []}
             bucket-two))
      (is (= {:id (:id bucket-two)
              :name (str (:id bucket-two) "-bucket")
              :meta {}
              :result 42
              :error [nil nil]
              :logs []}
             consolidated)
          "consolidate flattens nested bucket to extract inner result"))))

(deftest consolidate-merges-logs
  (testing "consolidate merges outer and inner logs"
    (let [outer {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Outer"}
          inner {:indent 1 :time (Instant/parse "2024-01-15T10:30:01Z") :level :debug :value "Inner"}
          bucket-one (bucket/grab :ok :logs [inner])
          bucket-two (bucket/grab bucket-one :logs [outer])
          consolidated (bucket/consolidate bucket-two)]
      (is (= {:id (:id bucket-one)
              :name (str (:id bucket-one) "-bucket")
              :meta {}
              :result :ok
              :error [nil nil]
              :logs [inner]}
             bucket-one))
      (is (= {:id (:id bucket-two)
              :name (str (:id bucket-two) "-bucket")
              :meta {}
              :result bucket-one
              :error [nil nil]
              :logs [outer]}
             bucket-two))
      (is (= {:id (:id bucket-two)
              :name (str (:id bucket-two) "-bucket")
              :meta {}
              :result :ok
              :error [nil nil]
              :logs [outer inner]}
             consolidated)
          "consolidate merges logs from outer and inner buckets"))))

(deftest consolidate-propagates-errors
  (testing "consolidate propagates inner bucket errors"
    (let [ex (RuntimeException. "err")
          bucket-one (bucket/grab :error [ex "ctx"])
          bucket-two (bucket/grab bucket-one)
          consolidated (bucket/consolidate bucket-two)]
      (is (= {:id (:id bucket-two)
              :name (str (:id bucket-two) "-bucket")
              :meta {}
              :result nil
              :error [ex "ctx"]
              :logs []}
             consolidated)
          "consolidate extracts and propagates error from inner bucket"))))

(deftest consolidate-metadata-behavior
  (testing "consolidate preserves outer bucket metadata when only outer has metadata"
    (let [bucket-one (bucket/grab 42)
          bucket-two (bucket/grab bucket-one :meta {:outer :data :level :shallow})
          consolidated (bucket/consolidate bucket-two)]
      (is (= {}
             (:meta bucket-one))
          "inner bucket has no metadata")
      (is (= {:outer :data :level :shallow}
             (:meta bucket-two))
          "outer bucket has metadata")
      (is (= {:outer :data :level :shallow}
             (:meta consolidated))
          "consolidate preserves outer bucket metadata")))

  (testing "consolidate uses outer metadata when both buckets have metadata"
    (let [bucket-one (bucket/grab 42 :meta {:inner :data :source :db})
          bucket-two (bucket/grab bucket-one :meta {:outer :data :source :api})
          consolidated (bucket/consolidate bucket-two)]
      (is (= {:inner :data :source :db}
             (:meta bucket-one))
          "inner bucket has its own metadata")
      (is (= {:outer :data :source :api}
             (:meta bucket-two))
          "outer bucket has different metadata")
      (is (= {:outer :data :source :api}
             (:meta consolidated))
          "consolidate uses outer bucket metadata, ignoring inner metadata"))))
