(ns monad.laws.left-identity-test
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]]
            [monad :as monad])
  (:import [java.time Instant]))

(deftest left-identity
  (testing "pure a >>= f equals f a"
    (let [f (fn [x] (monad/pure (* x 2)))
          monad-one (monad/pure 21)
          left (monad/bind monad-one f)
          right (f 21)]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :value 21
              :error [nil nil]
              :logs []}
             monad-one))
      (is (not= (:id left) (:id right)))
      (is (= (select-keys right [:value :error :logs :meta])
             (select-keys left [:value :error :logs :meta])))
      (is (= {:id (:id left)
              :name (str (:id left) "-bucket")
              :meta {}
              :value 42
              :error [nil nil]
              :logs []}
             left)
          "left identity law: binding pure value to f is equivalent to applying f")))

  (testing "left identity with logs and errors"
    (let [l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "run"}
          f-ok (fn [s] (monad/pure (str s "!") :logs [l]))
          f-bad (fn [_] (bucket/grab :error ["bad"]))
          monad-one (monad/pure "x")
          monad-two (monad/pure :x)
          r-ok (monad/bind monad-one f-ok)
          r-bad (monad/bind monad-two f-bad)]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :value "x"
              :error [nil nil]
              :logs []}
             monad-one))
      (is (= {:id (:id monad-two)
              :name (str (:id monad-two) "-bucket")
              :meta {}
              :value :x
              :error [nil nil]
              :logs []}
             monad-two))
      (is (= {:id (:id r-ok)
              :name (str (:id r-ok) "-bucket")
              :meta {}
              :value "x!"
              :error [nil nil]
              :logs [l]}
             r-ok))
      (is (= (select-keys (f-bad :x) [:value :error :logs :meta])
             (select-keys r-bad [:value :error :logs :meta]))
          "left identity holds for both success and error cases"))))
