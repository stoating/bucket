(ns monad.combinators.kleisli-composition-test
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]]
            [monad :as monad]))

(deftest kleisli-basic
  (testing "two functions composed"
    (let [f (fn [x] (monad/pure (+ x 5)))
          g (fn [x] (monad/pure (* x 2)))
          h (monad/>> f g)
          r (h 3)]
      (is (= {:id (:id r)
              :name (str (:id r) "-bucket")
              :meta {}
              :result 16
              :error [nil nil]
              :logs []}
             r)
          ">> composes monadic functions left-to-right")))

  (testing "identity law with pure"
    (let [f (fn [x] (monad/pure (* x 2)))
          h (monad/>> f monad/pure)
          r1 (f 10)
          r2 (h 10)]
      (is (= (select-keys r1 [:result :error :logs :meta])
             (select-keys r2 [:result :error :logs :meta]))
          ">> f pure behaves the same as f alone"))))

(deftest kleisli-errors
  (testing "first function error stops composition"
    (let [bad (fn [_] (bucket/grab :error ["bad"]))
          never (fn [_] (monad/pure :never))
          h (monad/>> bad never)
          r (h :x)]
      (is (= {:id (:id r)
              :name (str (:id r) "-bucket")
              :meta {}
              :result nil
              :error ["bad"]
              :logs []}
             r)
          ">> short-circuits on first error without calling subsequent functions"))))
