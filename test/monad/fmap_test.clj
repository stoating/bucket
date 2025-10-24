(ns monad.fmap-test
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]]
            [monad :as monad])
  (:import [java.time Instant]))

(deftest fmap-basic
  (testing "value transformed"
    (let [monad-one (monad/pure 10)
          result (monad/fmap #(* % 2) monad-one)]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result 10
              :error [nil nil]
              :logs []}
             monad-one))
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result 20
              :error [nil nil]
              :logs []}
             result)
          "fmap applies function to wrapped value")))

  (testing "logs preserved during transform"
    (let [l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "x"}
          monad-one (monad/pure "abc" :logs [l])
          result (monad/fmap count monad-one)]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result "abc"
              :error [nil nil]
              :logs [l]}
             monad-one))
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result 3
              :error [nil nil]
              :logs [l]}
             result)
          "fmap preserves logs while transforming value"))))

(deftest fmap-errors
  (testing "error bypasses function"
    (let [err (bucket/grab :error ["bad"])]
      (is (= err (monad/fmap inc err))
          "fmap short-circuits on error without applying function"))))

