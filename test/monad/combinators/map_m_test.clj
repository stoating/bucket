(ns monad.combinators.map-m-test
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]]
            [monad :as monad])
  (:import [java.time Instant]))

(deftest map-m-success
  (testing "function applied to numbers"
    (let [log (fn [x] {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :debug :value (str "* " x)})
          f (fn [x] (monad/pure (* x 2) :logs [(log x)]))
          res (monad/map-m f [1 2 3])]
      (is (= {:id (:id res)
              :name (str (:id res) "-bucket")
              :meta {}
              :result [2 4 6]
              :error [nil nil]
              :logs [{:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :debug :value "* 1"}
                     {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :debug :value "* 2"}
                     {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :debug :value "* 3"}]}
             res)
          "map-m applies function to each element and collects results with logs")))

  (testing "empty list handled"
    (let [res (monad/map-m (fn [x] (monad/pure x)) [])]
      (is (= {:id (:id res)
              :name (str (:id res) "-bucket")
              :meta {}
              :result []
              :error [nil nil]
              :logs []}
             res)
          "map-m returns empty result for empty input"))))

(deftest map-m-fails-fast
  (testing "error on first element"
    (let [f (fn [n] (if (pos? n)
                      (monad/pure n)
                      (bucket/grab :error ["must be positive"])))
          res (monad/map-m f [-1 2 3])]
      (is (= {:id (:id res)
              :name (str (:id res) "-bucket")
              :meta {}
              :result nil
              :error ["must be positive"]
              :logs []}
             res)
          "map-m stops at first error")))

  (testing "error after successful elements preserves prior logs"
    (let [l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "x"}
          f (fn [s]
              (if (= s "bad")
                (bucket/grab nil :logs [l] :error ["boom"])
                (monad/pure (str s "!") :logs [l])))
          res (monad/map-m f ["a" "bad" "c"])]
      (is (= {:id (:id res)
              :name (str (:id res) "-bucket")
              :meta {}
              :result nil
              :error ["boom"]
              :logs [l l]}
             res)
          "map-m preserves logs from successful elements before error"))))
