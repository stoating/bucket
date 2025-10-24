(ns bucket.stir-in-test
  "Bucket/stir-in - apply pure function to value."
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(deftest stir-in-value-transformation-test
  (testing "numeric transformation"
    (let [bucket-one (bucket/grab 10)
          times-2 (fn [x] (* x 2))
          stirred-bucket (bucket/stir-in times-2 bucket-one)]
      (is (= {:id (:id bucket-one)
              :name (str (:id bucket-one) "-bucket")
              :meta {}
              :result 10
              :error [nil nil]
              :logs []}
             bucket-one))
      (is (= {:id (:id bucket-one)
              :name (str (:id bucket-one) "-bucket")
              :meta {}
              :result 20
              :error [nil nil]
              :logs []}
             stirred-bucket))))

  (testing "string length"
    (let [bucket-one (bucket/grab "hello world")
          stirred-bucket (bucket/stir-in count bucket-one)]
      (is (= {:id (:id bucket-one)
              :name (str (:id bucket-one) "-bucket")
              :meta {}
              :result "hello world"
              :error [nil nil]
              :logs []}
             bucket-one))
      (is (= {:id (:id bucket-one)
              :name (str (:id bucket-one) "-bucket")
              :meta {}
              :result 11
              :error [nil nil]
              :logs []}
             stirred-bucket))))

  (testing "logs preserved"
    (let [logs [{:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Data loaded"}]
          bucket-one (bucket/grab "hi" :logs logs)
          stirred-bucket (bucket/stir-in count bucket-one)]
      (is (= {:id (:id bucket-one)
              :name (str (:id bucket-one) "-bucket")
              :meta {}
              :result "hi"
              :error [nil nil]
              :logs logs}
             bucket-one))
      (is (= {:id (:id bucket-one)
              :name (str (:id bucket-one) "-bucket")
              :meta {}
              :result 2
              :error [nil nil]
              :logs logs}
             stirred-bucket))))

  (testing "error short-circuit"
    (let [ioe (java.io.IOException. "fail")
          err (bucket/grab :error [ioe "Failed"])
          result (bucket/stir-in inc err)]
      (is (= err result)))))

(deftest stir-in-metadata-behavior
  (testing "metadata preserved from input bucket"
    (let [bucket-one (bucket/grab 10 :meta {:source :api :version 1})
          times-2 (fn [x] (* x 2))
          stirred-bucket (bucket/stir-in times-2 bucket-one)]
      (is (= {:source :api :version 1}
             (:meta bucket-one))
          "original bucket should have metadata")
      (is (= {:source :api :version 1}
             (:meta stirred-bucket))
          "stir-in preserves input bucket metadata")))

  (testing "metadata preserved when input has metadata but pure function doesn't affect it"
    (let [logs [{:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Data loaded"}]
          bucket-one (bucket/grab "hello" :logs logs  :meta {:user "alice" :session-id "xyz"})
          stirred-bucket (bucket/stir-in count bucket-one)]
      (is (= {:user "alice" :session-id "xyz"}
             (:meta stirred-bucket))
          "metadata persists through stir-in with logs"))))
