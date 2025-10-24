(ns monad.combinators.join-test
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]]
            [monad :as monad])
  (:import [java.time Instant]))

(deftest join-basic
  (testing "nested value flattened"
    (let [monad-one (monad/pure 42)
          monad-two (monad/pure monad-one)
          res (monad/join monad-two)]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result 42
              :error [nil nil]
              :logs []}
             monad-one))
      (is (= {:id (:id monad-two)
              :name (str (:id monad-two) "-bucket")
              :meta {}
              :result {:id (:id monad-one)
                       :name (str (:id monad-one) "-bucket")
                       :meta {}
                       :result 42
                       :error [nil nil]
                       :logs []}
              :error [nil nil]
              :logs []}
             monad-two))
      (is (= {:id (:id monad-two)
              :name (str (:id monad-two) "-bucket")
              :meta {}
              :result 42
              :error [nil nil]
              :logs []}
             res)
          "join extracts inner result from nested monad")))

  (testing "logs merged outer then inner"
    (let [l1 {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "outer"}
          l2 {:indent 1 :time (Instant/parse "2024-01-15T10:30:01Z") :level :debug :value "inner"}
          monad-one (monad/pure :ok :logs [l2])
          monad-two (monad/pure monad-one :logs [l1])
          res (monad/join monad-two)]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result :ok
              :error [nil nil]
              :logs [l2]}
             monad-one))
      (is (= {:id (:id monad-two)
              :name (str (:id monad-two) "-bucket")
              :meta {}
              :result {:id (:id monad-one)
                       :name (str (:id monad-one) "-bucket")
                       :meta {}
                       :result :ok
                       :error [nil nil]
                       :logs [l2]}
              :error [nil nil]
              :logs [l1]}
             monad-two))
      (is (= {:id (:id monad-two)
              :name (str (:id monad-two) "-bucket")
              :meta {}
              :result :ok
              :error [nil nil]
              :logs [l1 l2]}
             res)
          "join concatenates outer logs before inner logs"))))

(deftest join-errors
  (testing "outer error returned as-is"
    (let [err (bucket/grab :error ["boom"])
          r (monad/join err)]
      (is (= err r)
          "join returns outer error without unwrapping")))

  (testing "inner error surfaces with outer logs"
    (let [l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "before"}
          inner (bucket/grab :error ["bad"])
          res (monad/join (monad/pure inner :logs [l]))]
      (is (= {:id (:id res)
              :name (str (:id res) "-bucket")
              :meta {}
              :result nil
              :error ["bad"]
              :logs [l]}
             res)
          "join surfaces inner error while preserving outer logs"))))
