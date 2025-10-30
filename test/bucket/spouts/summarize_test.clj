(ns bucket.spouts.summarize-test
  "Tests for bucket spouts summarize function."
  (:require [bucket :as bucket]
            [bucket.spouts.reserve :as spouts]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(deftest summarize-successful-bucket-test
  (testing "summarizes successful bucket"
    (let [logs [{:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Test1"}
                {:indent 1 :time (Instant/parse "2024-01-15T10:30:01Z") :level :debug :value "Test2"}]
          bucket (bucket/grab {:user "alice"} :logs logs)
          summary (spouts/summarize bucket)]
      (is (= {:id (:id bucket)
              :name (:name bucket)
              :meta (:meta bucket)
              :log-count 2
              :error-type nil
              :value-type :map}
             summary)
          "summarize extracts log count, error status, and value type for successful bucket"))))

(deftest summarize-error-bucket-test
  (testing "summarizes bucket with exception"
    (let [ex (ex-info "Test error" {})
          bucket (bucket/grab :error [ex nil])
          summary (spouts/summarize bucket)]
      (is (= {:id (:id bucket)
              :name (:name bucket)
              :meta (:meta bucket)
              :log-count 0
              :error-type :exception
              :value-type :nil}
             summary)
          "summarize identifies exception errors and nil values"))))

(deftest summarize-message-error-bucket-test
  (testing "summarizes bucket with message error"
    (let [bucket (bucket/grab :error [nil "error message"])
          summary (spouts/summarize bucket)]
      (is (= {:id (:id bucket)
              :name (:name bucket)
              :meta (:meta bucket)
              :log-count 0
              :error-type :message
              :value-type :nil}
             summary)
          "summarize identifies message-only errors"))))

(deftest summarize-value-types-test
  (testing "identifies different value types correctly"
    (is (= :string (:value-type (spouts/summarize (bucket/grab "text")))))
    (is (= :number (:value-type (spouts/summarize (bucket/grab 42)))))
    (is (= :boolean (:value-type (spouts/summarize (bucket/grab true)))))
    (is (= :vector (:value-type (spouts/summarize (bucket/grab [1 2 3])))))
    (is (= :seq (:value-type (spouts/summarize (bucket/grab '(1 2 3)))))
        "summarize correctly identifies string, number, boolean, vector, and seq value types")))
