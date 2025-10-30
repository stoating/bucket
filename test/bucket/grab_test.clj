(ns bucket.grab-test
  "Bucket/grab - unit constructor for Bucket values."
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]]
            [test-helpers :as th])
  (:import [java.time Instant]))

(deftest grab-wraps-values-test
  (testing "simple value wrapping"
    (let [grabbed-bucket (bucket/grab 42)]
      (is (= {:id (:id grabbed-bucket)
              :name (str (:id grabbed-bucket) "-bucket")
              :meta {}
              :value 42
              :error [nil nil]
              :logs []}
             grabbed-bucket))))

  (testing "string value wrapping"
    (let [grabbed-bucket (bucket/grab "hello world")]
      (is (= {:id (:id grabbed-bucket)
              :name (str (:id grabbed-bucket) "-bucket")
              :meta {}
              :value "hello world"
              :error [nil nil]
              :logs []}
             grabbed-bucket))))

  (testing "complex data structure wrapping"
    (let [input {:user-id 123 :name "Alice" :roles [:admin :user]}
          grabbed-bucket (bucket/grab input)]
      (is (= {:id (:id grabbed-bucket)
              :name (str (:id grabbed-bucket) "-bucket")
              :meta {}
              :value {:user-id 123
                       :name "Alice"
                       :roles [:admin :user]}
              :error [nil nil]
              :logs []}
             grabbed-bucket))))

  (testing "function value wrapping (by reference)"
    (let [add-one (fn [x] (inc x))
          grabbed-bucket (bucket/grab add-one)]
      (is (= {:id (:id grabbed-bucket)
              :name (str (:id grabbed-bucket) "-bucket")
              :meta {}
              :value add-one
              :error [nil nil]
              :logs []}
             grabbed-bucket))
      (is (fn? (:value grabbed-bucket))))))

(deftest grab-with-logs-test
  (testing "single log entry"
    (let [log {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "User authenticated"}
          grabbed-bucket (bucket/grab "user-token-xyz" :logs [log])]
      (is (= {:id (:id grabbed-bucket)
              :name (str (:id grabbed-bucket) "-bucket")
              :meta {}
              :value "user-token-xyz"
              :error [nil nil]
              :logs [log]}
             grabbed-bucket))))

  (testing "multiple logs"
    (let [logs [{:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Starting process"}
                {:indent 1 :time (Instant/parse "2024-01-15T10:30:01Z") :level :debug :value "Validating input"}]
          grabbed-bucket (bucket/grab {:status "ok"} :logs logs)]
      (is (= {:id (:id grabbed-bucket)
              :name (str (:id grabbed-bucket) "-bucket")
              :meta {}
              :value {:status "ok"}
              :error [nil nil]
              :logs logs}
             grabbed-bucket))))

  (testing "empty logs"
    (let [grabbed-bucket (bucket/grab "data")]
      (is (= {:id (:id grabbed-bucket)
              :name (str (:id grabbed-bucket) "-bucket")
              :meta {}
              :value "data"
              :error [nil nil]
              :logs []}
             grabbed-bucket)))))

(deftest grab-preserves-nil-values-test
  (testing "nil as value"
    (let [grabbed-bucket (bucket/grab nil)]
      (is (= {:id (:id grabbed-bucket)
              :name (str (:id grabbed-bucket) "-bucket")
              :meta {}
              :value nil
              :error [nil nil]
              :logs []}
             grabbed-bucket))))

  (testing "nil with logs"
    (let [log {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "No data found"}
          grabbed-bucket (bucket/grab nil :logs [log])]
      (is (= {:id (:id grabbed-bucket)
              :name (str (:id grabbed-bucket) "-bucket")
              :meta {}
              :value nil
              :error [nil nil]
              :logs [log]}
             grabbed-bucket)))))

(deftest grab-no-args-test
  (testing "no arguments at all"
    (let [grabbed-bucket (bucket/grab)]
      (is (= {:id (:id grabbed-bucket)
              :name (str (:id grabbed-bucket) "-bucket")
              :meta {}
              :value nil
              :error [nil nil]
              :logs []}
             grabbed-bucket)))))

(deftest grab-with-explicit-error-test
  (testing "value + logs + error creates a fail bucket"
    (let [ts (Instant/parse "2024-01-15T10:30:00Z")
          log [{:indent 0 :time ts :level :info :value "prep"}]
          ex (ex-info "boom" {:k :v})
          grabbed-bucket (bucket/grab nil :logs log :error [ex "ctx"])]
      (th/assert-error grabbed-bucket)
      (is (= {:id (:id grabbed-bucket)
              :name (str (:id grabbed-bucket) "-bucket")
              :meta {}
              :value nil
              :error [ex "ctx"]
              :logs log}
             grabbed-bucket)))))

(deftest grab-with-metadata-test
  (testing "simple metadata"
    (let [grabbed-bucket (bucket/grab 42 :meta {:source :api :version 1})]
      (th/assert-no-error grabbed-bucket)
      (is (= {:source :api :version 1}
             (:meta grabbed-bucket))
          "bucket should contain specified metadata")))

  (testing "metadata with logs"
    (let [log {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Processing"}
          grabbed-bucket (bucket/grab "data" :logs [log] :meta {:user "alice" :request-id "xyz-123"})]
      (th/assert-no-error grabbed-bucket)
      (is (= {:user "alice" :request-id "xyz-123"}
             (:meta grabbed-bucket))
          "metadata should be preserved alongside logs"))))
