(ns monad.pure-test
  (:require [clojure.test :refer [deftest is testing]]
            [monad :as monad])
  (:import [java.time Instant]))

(deftest pure-wraps
  (testing "simple values wrapped"
    (let [monad-one (monad/pure 42)
          monad-two (monad/pure "hello")]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :value 42
              :error [nil nil]
              :logs []}
             monad-one))
      (is (= {:id (:id monad-two)
              :name (str (:id monad-two) "-bucket")
              :meta {}
              :value "hello"
              :error [nil nil]
              :logs []}
             monad-two)
          "pure wraps values in monad structure")))

  (testing "value with logs"
    (let [l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "ok"}
          monad-one (monad/pure {:a 1} :logs [l])]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :value {:a 1}
              :error [nil nil]
              :logs [l]}
             monad-one)
          "pure includes logs when provided")))

  (testing "explicit error bucket"
    (let [ex (ex-info "boom" {:k :v})
          l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :error :value "failed"}
          monad-one (monad/pure nil :logs [l] :error [ex "ctx"])]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :value nil
              :error [ex "ctx"]
              :logs [l]}
             monad-one)
          "pure can create error buckets with explicit error parameter"))))
