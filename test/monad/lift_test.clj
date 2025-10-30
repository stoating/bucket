(ns monad.lift-test
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]]
            [monad :as monad]))

(deftest lift-basic
  (testing "lifted function applied"
    (let [f (monad/lift (fn [x] (str x "!")))
          r (f (monad/pure "A"))]
      (is (= {:id (:id r)
              :name (str (:id r) "-bucket")
              :meta {}
              :value "A!"
              :error [nil nil]
              :logs []}
             r)
          "lift creates a monadic function from a regular function")))

  (testing "lifted function on error"
    (let [err (bucket/grab :error ["nope"])
          f (monad/lift inc)
          r (f err)]
      (is (= err r)
          "lifted function short-circuits on error"))))
