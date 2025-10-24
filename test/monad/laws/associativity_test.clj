(ns monad.laws.associativity-test
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]]
            [monad :as monad])
  (:import [java.time Instant]))

(deftest associativity
  (testing "(m >>= f) >>= g equals m >>= (λx. f x >>= g)"
    (let [monad-one (monad/pure 5)
          f (fn [x] (monad/pure (+ x 10)))
          g (fn [x] (monad/pure (* x 3)))
          left (-> monad-one
                   (monad/bind f)
                   (monad/bind g))
          right (monad/bind monad-one (fn [x] (monad/bind (f x) g)))]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result 5
              :error [nil nil]
              :logs []}
             monad-one))
      (is (= left right))
      (is (= {:id (:id left)
              :name (str (:id left) "-bucket")
              :meta {}
              :result 45
              :error [nil nil]
              :logs []}
             left)
          "associativity law: order of bind operations doesn't affect result")))

  (testing "associativity with logs and errors"
    (let [l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "step"}
          monad-one (monad/pure "ok")
          f (fn [s] (monad/pure (str s "1") :logs [l]))
          g (fn [_] (bucket/grab :error ["boom"]))
          left (-> monad-one (monad/bind f) (monad/bind g))
          right (monad/bind monad-one (fn [x] (monad/bind (f x) g)))]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result "ok"
              :error [nil nil]
              :logs []}
             monad-one))
      (is (= left right))
      (is (= {:id (:id left)
              :name (str (:id left) "-bucket")
              :meta {}
              :result nil
              :error ["boom"]
              :logs [l]}
             left)
          "associativity holds with logs and error handling"))))
