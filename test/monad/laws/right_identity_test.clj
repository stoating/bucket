(ns monad.laws.right-identity-test
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]]
            [monad :as monad])
  (:import [java.time Instant]))

(deftest right-identity
  (testing "m >>= pure equals m (success case)"
    (let [l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "ok"}
          monad-one (monad/pure {:a 1} :logs [l])
          r (monad/bind monad-one monad/pure)]
      (is (= (select-keys monad-one [:value :error :logs :meta])
             (select-keys r [:value :error :logs :meta])))
      (is (= {:id (:id r)
              :name (str (:id r) "-bucket")
              :meta {}
              :value {:a 1}
              :error [nil nil]
              :logs [l]}
             r)
          "right identity law: binding to pure preserves the monad")))

  (testing "m >>= pure equals m (error case)"
    (let [err (bucket/grab :error ["bad"])
          r (monad/bind err monad/pure)]
      (is (= err r)
          "right identity holds for error cases"))))
