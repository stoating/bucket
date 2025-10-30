(ns bucket.pass-test
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(deftest pass-basic
  (let [bucket-one (bucket/grab 5)
        result (bucket/pass bucket-one (fn [x] (bucket/grab (* x 2))))]
    (is (= {:id (:id bucket-one)
            :name (str (:id bucket-one) "-bucket")
            :meta {}
            :value 5
            :error [nil nil]
            :logs []}
           bucket-one))
    (is (= {:id (:id bucket-one)
            :name (str (:id bucket-one) "-bucket")
            :meta {}
            :value 10
            :error [nil nil]
            :logs []}
           result)))

  (let [bucket-one (bucket/grab "hello")
        append-world (fn [s] (bucket/grab (str s " world")))
        to-upper (fn [s] (bucket/grab (.toUpperCase s)))
        result (-> bucket-one
                   (bucket/pass append-world)
                   (bucket/pass to-upper))]
    (is (= {:id (:id bucket-one)
            :name (str (:id bucket-one) "-bucket")
            :meta {}
            :value "hello"
            :error [nil nil]
            :logs []}
           bucket-one))
    (is (= {:id (:id bucket-one)
            :name (str (:id bucket-one) "-bucket")
            :meta {}
            :value "HELLO WORLD"
            :error [nil nil]
            :logs []}
           result))))

(deftest pass-logs
  (let [l1 {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Step1"}
        l2 {:indent 1 :time (Instant/parse "2024-01-15T10:30:01Z") :level :debug :value "Step2"}
        bucket-one (bucket/grab 1 :logs [l1])
        inc-with-log (fn [n] (bucket/grab (inc n) :logs [l2]))
        result (-> bucket-one
                   (bucket/pass inc-with-log))]
    (is (= {:id (:id bucket-one)
            :name (str (:id bucket-one) "-bucket")
            :meta {}
            :value 1
            :error [nil nil]
            :logs [l1]}
           bucket-one))
    (is (= {:id (:id bucket-one)
            :name (str (:id bucket-one) "-bucket")
            :meta {}
            :value 2
            :error [nil nil]
            :logs [l1 l2]}
           result))))

(deftest pass-errors
  (let [ex (ex-info "Invalid" {})
        err (bucket/grab :error [ex "ctx"])
        res (bucket/pass err (fn [_] (bucket/grab :never)))]
    (is (= err res))))

(deftest pass-metadata-behavior
  (testing "metadata preserved when only input bucket has metadata"
    (let [bucket-one (bucket/grab 5 :meta {:source :db :query-id 123})
          result (bucket/pass bucket-one (fn [x] (bucket/grab (* x 2))))]
      (is (= {:source :db :query-id 123}
             (:meta bucket-one))
          "original bucket should have metadata")
      (is (= {:source :db :query-id 123}
             (:meta result))
          "pass preserves input bucket metadata")))

  (testing "metadata from input bucket preserved even when function returns bucket with different metadata"
    (let [bucket-one (bucket/grab 10 :meta {:user "bob" :action "calculate"})
          with-meta-fn (fn [x] (bucket/grab (* x 2) :meta {:computed true :timestamp 9999}))
          result (bucket/pass bucket-one with-meta-fn)]
      (is (= {:user "bob" :action "calculate"}
             (:meta result))
          "pass uses input bucket metadata, ignoring function's output bucket metadata")))

  (testing "metadata preserved through multiple passes"
    (let [bucket-one (bucket/grab 10 :meta {:session-id "xyz" :version 1})
          add-five (fn [x] (bucket/grab (+ x 5) :meta {:step :add}))
          times-two (fn [x] (bucket/grab (* x 2) :meta {:step :multiply}))
          result (-> bucket-one
                     (bucket/pass add-five)
                     (bucket/pass times-two))]
      (is (= {:session-id "xyz" :version 1}
             (:meta result))
          "metadata from original bucket persists through chain, ignoring intermediate metadata"))))
