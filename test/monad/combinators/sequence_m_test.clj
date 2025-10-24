(ns monad.combinators.sequence-m-test
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]]
            [monad :as monad])
  (:import [java.time Instant]))

(deftest sequence-m-success-and-logs
  (testing "results collected in order"
    (let [monad-one (monad/pure 1)
          monad-two (monad/pure 2)
          monad-three (monad/pure 3)
          r (monad/sequence-m [monad-one monad-two monad-three])]
      (is (not= (:id monad-one) (:id monad-two) (:id monad-three) (:id r)))
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result 1
              :error [nil nil]
              :logs []}
             monad-one))
      (is (= {:id (:id monad-two)
              :name (str (:id monad-two) "-bucket")
              :meta {}
              :result 2
              :error [nil nil]
              :logs []}
             monad-two))
      (is (= {:id (:id monad-three)
              :name (str (:id monad-three) "-bucket")
              :meta {}
              :result 3
              :error [nil nil]
              :logs []}
             monad-three))
      (is (= {:id (:id r)
              :name (str (:id r) "-bucket")
              :meta {}
              :result [1 2 3]
              :error [nil nil]
              :logs []}
             r)
          "sequence-m combines monadic values preserving order")))

  (testing "logs accumulated from each item"
    (let [l1 {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "a"}
          l2 {:indent 0 :time (Instant/parse "2024-01-15T10:30:01Z") :level :info :value "b"}
          r (monad/sequence-m [(monad/pure :x :logs [l1]) (monad/pure :y :logs [l2])])]
      (is (= {:id (:id r)
              :name (str (:id r) "-bucket")
              :meta {}
              :result [:x :y]
              :error [nil nil]
              :logs [l1 l2]}
             r)
          "sequence-m concatenates logs from all items"))))

(deftest sequence-m-fails-fast
  (testing "first error encountered"
    (let [err (bucket/grab :error ["bad"])
          r (monad/sequence-m [(monad/pure 1) err (monad/pure 3)])]
      (is (= {:id (:id r)
              :name (str (:id r) "-bucket")
              :meta {}
              :result nil
              :error ["bad"]
              :logs []}
             r)
          "sequence-m stops at the first error without processing remaining items"))))
