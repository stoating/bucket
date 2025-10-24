(ns bucket-test
  (:require [bucket :as bucket]
            [bucket.spouts.chain :as chain]
            [bucket.spouts.extract :as extract]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [monad :as monad]
            [test-helpers :as th])
  (:import [java.time Instant]))

(use-fixtures :each th/clean-test-temp-fixture)

(defn- sample-logs []
  (let [ts (Instant/now)]
    [{:indent 0 :time ts :level :info :value "first"}
     {:indent 2 :time ts :level :warning :value "second"}]))

(deftest spill-prints-and-returns
  (testing "spill prints logs to stdout and returns result"
    (let [resp (monad/pure :ok :logs (sample-logs))
          out-str (with-out-str
                    (let [ret (extract/spill resp :log-out :stdout :meta-out :none :out-dir th/test-temp-root)]
                      (is (= :ok ret))))]
      (is (re-find #"INFO" out-str))
      (is (re-find #"WARNING" out-str)
          "spill prints all log levels and returns the bucket result"))))

(deftest spill-handles-error-without-exit-when-nil
  (testing "spill handles errors gracefully when :require-result is false"
    (let [e (ex-info "boom" {})
          resp (bucket/grab :error [e nil])
          out-str (with-out-str
                    (let [ret (extract/spill resp :log-out :stdout :meta-out :none :out-dir th/test-temp-root :exit :continue :require-result false)]
                      (is (nil? ret))))]
      (is (re-find #"error class" out-str)
          "spill prints error information and returns nil without exiting"))))

(deftest pour-into-combines-logs-purely
  (testing "pour-into combines results and logs from two buckets"
    (let [ts (Instant/now)
          old-bucket (monad/pure :res :logs [{:indent 0 :time ts :level :info :value "child"}])
          new-bucket (monad/pure :new-value)
          combined (chain/pour-into new-bucket old-bucket)]
      (is (= [:res :new-value] (:result combined)))
      (is (= (:id new-bucket) (:id combined)))
      (is (= [{:indent 0 :time ts :level :info :value "child"}]
             (:logs combined))
          "pour-into gathers results, uses new bucket id, and combines logs"))))

(deftest pour-into-respects-base-indent
  (testing "pour-into preserves log order by timestamp with base indent"
    (let [ts1 (Instant/now)
          ts2 (.plusSeconds ts1 1)
          old-bucket (monad/pure :ok :logs [{:indent 0 :time ts2 :level :info :value "child"}])
          new-bucket (monad/pure :new-value :logs [{:indent 2 :time ts1 :level :info :value "base"}])
          combined (chain/pour-into new-bucket old-bucket)]
      (is (= [:ok :new-value] (:result combined)))
      (is (= 2 (count (:logs combined))))
      (is (= "base" (:value (first (:logs combined)))))
      (is (= "child" (:value (second (:logs combined))))
          "pour-into sorts combined logs by timestamp, base log first"))))

(deftest pour-into-metadata-behavior
  (testing "metadata from new bucket with merge (default)"
    (let [ts (Instant/now)
          old-bucket (monad/pure :res :logs [{:indent 0 :time ts :level :info :value "child"}] :meta {:old :data})
          new-bucket (monad/pure :new-value :meta {:new :data})
          combined (chain/pour-into new-bucket old-bucket)]
      (is (= {:old :data :new :data}
             (:meta combined))
          "pour-into merges metadata by default")))

  (testing "metadata snapshot mode creates :previous-buckets vector"
    (let [ts1 (Instant/now)
          ts2 (.plusSeconds ts1 1)
          old-bucket (monad/pure :ok :logs [{:indent 0 :time ts2 :level :info :value "child"}] :meta {:source :db :version 1})
          new-bucket (monad/pure :new-value :logs [{:indent 2 :time ts1 :level :info :value "base"}] :meta {:destination :api})
          old-id (:id old-bucket)
          combined (chain/pour-into new-bucket old-bucket :meta-merge-type :snapshot)]
      (is (= {:destination :api
              :previous-buckets [{old-id {:source :db :version 1}}]}
             (:meta combined))
          "pour-into in snapshot mode creates :previous-buckets vector with old bucket metadata"))))
