(ns bucket.combinators.gather-test
  "Bucket/gather - collect sequence of Buckets into one Bucket containing results."
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(deftest gather-success-cases
  (testing "simple numbers"
    (let [bucket-one (bucket/grab 1)
          bucket-two (bucket/grab 2)
          bucket-three (bucket/grab 3)
          result (bucket/gather [bucket-one bucket-two bucket-three])]
      (is (not= (:id bucket-one) (:id bucket-two) (:id bucket-three) (:id result)))
      (is (= {:id (:id bucket-one)
              :name (str (:id bucket-one) "-bucket")
              :meta {}
              :result 1
              :error [nil nil]
              :logs []}
             bucket-one))
      (is (= {:id (:id bucket-two)
              :name (str (:id bucket-two) "-bucket")
              :meta {}
              :result 2
              :error [nil nil]
              :logs []}
             bucket-two))
      (is (= {:id (:id bucket-three)
              :name (str (:id bucket-three) "-bucket")
              :meta {}
              :result 3
              :error [nil nil]
              :logs []}
             bucket-three))
      (is (= {:id (:id result)
              :name (str (:id result) "-bucket")
              :meta {}
              :result [1 2 3]
              :error [nil nil]
              :logs []}
             result)
          "gather collects results into vector with unique IDs")))

  (testing "order preserved and logs accumulated"
    (let [l1 {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "A"}
          l2 {:indent 0 :time (Instant/parse "2024-01-15T10:30:01Z") :level :info :value "B"}
          bucket-one (bucket/grab "x" :logs [l1])
          bucket-two (bucket/grab "y" :logs [l2])
          result (bucket/gather [bucket-one bucket-two])]
      (is (not= (:id bucket-one) (:id bucket-two) (:id result)))
      (is (= {:id (:id bucket-one)
              :name (str (:id bucket-one) "-bucket")
              :meta {}
              :result "x"
              :error [nil nil]
              :logs [l1]}
             bucket-one))
      (is (= {:id (:id bucket-two)
              :name (str (:id bucket-two) "-bucket")
              :meta {}
              :result "y"
              :error [nil nil]
              :logs [l2]}
             bucket-two))
      (is (= {:id (:id result)
              :name (str (:id result) "-bucket")
              :meta {}
              :result ["x" "y"]
              :error [nil nil]
              :logs [l1 l2]}
             result)
          "gather preserves order and accumulates logs from all buckets"))))

(deftest gather-error-cases
  (testing "fails fast on first error"
    (let [err (bucket/grab :error ["boom"])
          bucket-one (bucket/grab 1)
          bucket-two (bucket/grab 3)
          result (bucket/gather [bucket-one err bucket-two])]
      (is (not= (:id bucket-one) (:id bucket-two) (:id err) (:id result)))
      (is (= {:id (:id bucket-one)
              :name (str (:id bucket-one) "-bucket")
              :meta {}
              :result 1
              :error [nil nil]
              :logs []}
             bucket-one))
      (is (= {:id (:id bucket-two)
              :name (str (:id bucket-two) "-bucket")
              :meta {}
              :result 3
              :error [nil nil]
              :logs []}
             bucket-two))
      (is (= {:id (:id result)
              :name (str (:id result) "-bucket")
              :meta {}
              :result nil
              :error ["boom"]
              :logs []}
             result)
          "gather fails fast on first error without processing remaining buckets")))

  (testing "error with prior logs preserved"
    (let [l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "first"}
          ex (RuntimeException. "bad")
          err (bucket/grab :error [ex "ctx"])
          bucket-one (bucket/grab :ok :logs [l])
          result (bucket/gather [bucket-one err])]
      (is (not= (:id bucket-one) (:id err) (:id result)))
      (is (= {:id (:id bucket-one)
              :name (str (:id bucket-one) "-bucket")
              :meta {}
              :result :ok
              :error [nil nil]
              :logs [l]}
             bucket-one))
      (is (= {:id (:id result)
              :name (str (:id result) "-bucket")
              :meta {}
              :result nil
              :error [ex "ctx"]
              :logs [l]}
             result)
          "gather preserves logs from successful buckets before error"))))

(deftest gather-metadata-behavior
  (testing "gather creates new bucket with empty metadata when input buckets have metadata"
    (let [bucket-one (bucket/grab 1 :meta {:source :db})
          bucket-two (bucket/grab 2 :meta {:validated true})
          bucket-three (bucket/grab 3 :meta {:processed true})
          result (bucket/gather [bucket-one bucket-two bucket-three])]
      (is (= {:source :db}
             (:meta bucket-one)))
      (is (= {:validated true}
             (:meta bucket-two)))
      (is (= {:processed true}
             (:meta bucket-three)))
      (is (= {}
             (:meta result))
          "gather creates new bucket with empty metadata, does not accumulate from inputs")))

  (testing "gather with different metadata in each bucket"
    (let [l1 {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "A"}
          l2 {:indent 0 :time (Instant/parse "2024-01-15T10:30:01Z") :level :info :value "B"}
          bucket-one (bucket/grab "x" :logs [l1] :meta {:id 1 :stage :first})
          bucket-two (bucket/grab "y" :logs [l2] :meta {:id 2 :stage :second})
          result (bucket/gather [bucket-one bucket-two])]
      (is (= ["x" "y"]
             (:result result)))
      (is (= {}
             (:meta result))
          "metadata is not preserved or merged in gather, new bucket has empty meta"))))
