(ns monad.bind-test
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]]
            [monad :as monad])
  (:import [java.time Instant]))

(deftest bind-basic
  (testing "single monadic step"
    (let [monad-one (monad/pure 5)
          res (monad/bind monad-one (fn [x] (monad/pure (* x 2))))]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result 5
              :error [nil nil]
              :logs []}
             monad-one))
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result 10
              :error [nil nil]
              :logs []}
             res)
          "bind chains monadic computation")))

  (testing "multiple bind steps chained"
    (let [monad-one (monad/pure "hi")
          res (-> monad-one
                  (monad/bind (fn [s] (monad/pure (str s " there"))))
                  (monad/bind (fn [s] (monad/pure (.toUpperCase s)))))]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result "hi"
              :error [nil nil]
              :logs []}
             monad-one))
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result "HI THERE"
              :error [nil nil]
              :logs []}
             res)
          "bind supports sequential monadic operations"))))

(deftest bind-logs
  (testing "logs accumulated through bind"
    (let [l1 {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "a"}
          l2 {:indent 1 :time (Instant/parse "2024-01-15T10:30:01Z") :level :debug :value "b"}
          res (-> (monad/pure 5 :logs [l1])
                  (monad/bind (fn [x] (monad/pure (+ x 7) :logs [l2]))))]
      (is (= {:id (:id res)
              :name (str (:id res) "-bucket")
              :meta {}
              :result 12
              :error [nil nil]
              :logs [l1 l2]}
             res)
          "bind concatenates logs from both steps"))))

(deftest bind-errors
  (testing "error stops computation"
    (let [err (bucket/grab :error ["bad"])
          res (monad/bind err (fn [_] (monad/pure :never)))]
      (is (= err res)
          "bind short-circuits on error without calling function")))

  (testing "error with prior logs retained"
    (let [l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "pre"}
          res (-> (monad/pure 1 :logs [l])
                  (monad/bind (fn [_] (bucket/grab :error ["boom"]))))]
      (is (= {:id (:id res)
              :name (str (:id res) "-bucket")
              :meta {}
              :result nil
              :error ["boom"]
              :logs [l]}
             res)
          "bind preserves logs from steps before error"))))
