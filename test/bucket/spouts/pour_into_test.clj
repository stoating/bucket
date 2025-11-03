(ns bucket.spouts.pour-into-test
  (:require [bucket :as bucket]
            [bucket.spouts :as spouts]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(deftest pour-into-result-gather-default
  (testing "result gathers both values by default"
    (let [ts (Instant/parse "2024-01-15T10:30:00Z")
          old-bucket (bucket/grab :old-value :logs [{:indent 0 :time ts :level :info :value "old"}])
          new-bucket (bucket/grab :new-value :logs [{:indent 0 :time ts :level :info :value "new"}])
          result (spouts/pour-into new-bucket old-bucket {:pour-type :gather})]
      (is (= [:old-value :new-value] (:value result))
          "result should gather both old and new values by default"))))

(deftest pour-into-pour-type-gather
  (testing "gather combines both values into vector"
    (let [old-bucket (bucket/grab 42)
          new-bucket (bucket/grab 99)
          result (spouts/pour-into new-bucket old-bucket :pour-type :gather)]
      (is (= [42 99] (:value result))
          "gather should create vector [old new]"))))

(deftest pour-into-pour-type-drop-old
  (testing "drop-old uses only new bucket's value"
    (let [old-bucket (bucket/grab :old-value)
          new-bucket (bucket/grab :new-value)
          result (spouts/pour-into new-bucket old-bucket :pour-type :drop)]
      (is (= :new-value (:value result))
          "drop-old should use new bucket's value only"))))

(deftest pour-into-pour-type-drop-new
  (testing "drop-new uses only old bucket's value"
    (let [old-bucket (bucket/grab :old-value)
          new-bucket (bucket/grab :new-value)
          result (spouts/pour-into old-bucket new-bucket :pour-type :drop)]
      (is (= :old-value (:value result))
          "drop-new should use old bucket's result only"))))

(deftest pour-into-pour-type-stir-in-old-to-new
  (testing "stir-in-old->new applies old result as function to new bucket"
    (let [old-bucket (bucket/grab inc) ; old result is a function
          new-bucket (bucket/grab 42) ; new result is a value
          result (spouts/pour-into new-bucket old-bucket)]
      (is (= 43 (:value result))
          "should apply old function (inc) to new bucket's value (42)")))

  (testing "stir-in-old->new with more complex function"
    (let [old-bucket (bucket/grab (fn [x] (* x 2)))
          new-bucket (bucket/grab 10)
          result (spouts/pour-into new-bucket old-bucket :pour-type :stir-in)]
      (is (= 20 (:value result))
          "should apply old function to new value"))))

(deftest pour-into-pour-type-stir-in-new-to-old
  (testing "stir-in-new->old applies new value as function to old bucket"
    (let [old-bucket (bucket/grab 42) ; old result is a value
          new-bucket (bucket/grab inc) ; new result is a function
          result (spouts/pour-into old-bucket new-bucket :pour-type :stir-in)]
      (is (= 43 (:value result))
          "should apply new function (inc) to old bucket's value (42)")))

  (testing "stir-in-new->old with string manipulation"
    (let [old-bucket (bucket/grab "hello")
          new-bucket (bucket/grab clojure.string/upper-case)
          result (spouts/pour-into old-bucket new-bucket :pour-type :stir-in)]
      (is (= "HELLO" (:value result))
          "should apply new function to old value"))))

(deftest pour-into-id-from-new-bucket
  (testing "id comes from new bucket, not old bucket"
    (let [ts (Instant/parse "2024-01-15T10:30:00Z")
          old-bucket (bucket/grab :old :logs [{:indent 0 :time ts :level :info :value "old"}])
          new-bucket (bucket/grab :new :logs [{:indent 0 :time ts :level :info :value "new"}])
          result (spouts/pour-into new-bucket old-bucket {:pour-type :gather})]
      (is (= (:id new-bucket) (:id result))
          "id should come from new bucket")
      (is (not= (:id old-bucket) (:id result))
          "id should not be from old bucket"))))

(deftest pour-into-name-from-new-bucket
  (testing "name comes from new bucket by default"
    (let [old-bucket (bucket/grab :old :name "old-bucket")
          new-bucket (bucket/grab :new :name "new-bucket")
          result (spouts/pour-into new-bucket old-bucket {:pour-type :gather})]
      (is (= "new-bucket" (:name result))
          "name should come from new bucket"))))

(deftest pour-into-custom-name
  (testing "name can be overridden with :new-name option"
    (let [old-bucket (bucket/grab :old :name "old-bucket")
          new-bucket (bucket/grab :new :name "new-bucket")
          result (spouts/pour-into new-bucket old-bucket :new-name "custom-bucket" :pour-type :gather)]
      (is (= "custom-bucket" (:name result))
          "name should be the custom name provided"))))

(deftest pour-into-logs-sorted-chronologically
  (testing "logs are combined and sorted by time (oldest to newest)"
    (let [ts1 (Instant/parse "2024-01-15T10:28:00Z")
          ts2 (Instant/parse "2024-01-15T10:29:00Z")
          ts3 (Instant/parse "2024-01-15T10:30:00Z")
          ts4 (Instant/parse "2024-01-15T10:31:00Z")
          old-bucket (bucket/grab :old
                                  :logs [{:indent 0 :time ts1 :level :info :value "first"}
                                         {:indent 1 :time ts3 :level :debug :value "third"}])
          new-bucket (bucket/grab :new
                                  :logs [{:indent 0 :time ts2 :level :info :value "second"}
                                         {:indent 2 :time ts4 :level :warn :value "fourth"}])
          result (spouts/pour-into new-bucket old-bucket {:pour-type :gather})]
      (is (= 4 (count (:logs result))))
      (is (= ["first" "second" "third" "fourth"]
             (mapv :value (:logs result)))
          "logs should be sorted by time in chronological order"))))

(deftest pour-into-logs-preserve-all-fields
  (testing "log sorting preserves all log fields (indent, level, etc)"
    (let [ts1 (Instant/parse "2024-01-15T10:30:00Z")
          ts2 (Instant/parse "2024-01-15T10:29:00Z")
          old-bucket (bucket/grab :old :logs [{:indent 5 :time ts1 :level :error :value "later"}])
          new-bucket (bucket/grab :new :logs [{:indent 2 :time ts2 :level :debug :value "earlier"}])
          result (spouts/pour-into new-bucket old-bucket {:pour-type :gather})]
      (is (= [{:indent 2 :time ts2 :level :debug :value "earlier"}
              {:indent 5 :time ts1 :level :error :value "later"}]
             (:logs result))
          "all log fields should be preserved during sorting"))))

(deftest pour-into-meta-merge-default
  (testing "metadata is merged by default (new overrides old)"
    (let [old-bucket (bucket/grab :old :meta {:source :api :version 1 :user "alice"})
          new-bucket (bucket/grab :new :meta {:destination :db :version 2})
          result (spouts/pour-into new-bucket old-bucket {:pour-type :gather})]
      (is (= {:source :api :version 2 :destination :db :user "alice"}
             (:meta result))
          "new bucket meta should override old bucket meta, but preserve unique keys"))))

(deftest pour-into-meta-merge-explicit
  (testing "metadata is merged when :meta-merge-type is :merge"
    (let [old-bucket (bucket/grab :old :meta {:a 1 :b 2 :c 3})
          new-bucket (bucket/grab :new :meta {:b 99 :d 4})
          result (spouts/pour-into new-bucket old-bucket :meta-merge-type :merge :pour-type :gather)]
      (is (= {:a 1 :b 99 :c 3 :d 4}
             (:meta result))
          "merge should combine both metas with new values taking precedence"))))

(deftest pour-into-meta-snapshot
  (testing "snapshot creates :previous-buckets vector with first old bucket"
    (let [old-bucket (bucket/grab :old :meta {:source :api :version 1})
          new-bucket (bucket/grab :new :meta {:destination :db})
          old-id (:id old-bucket)
          result (spouts/pour-into new-bucket old-bucket :meta-merge-type :snapshot :pour-type :gather)]
      (is (= {:destination :db
              :previous-buckets [{old-id {:source :api :version 1}}]}
             (:meta result))
          "snapshot should create :previous-buckets vector with old bucket metadata"))))

(deftest pour-into-meta-snapshot-empty-old-meta
  (testing "snapshot works correctly when old bucket has empty meta"
    (let [old-bucket (bucket/grab :old :meta {})
          new-bucket (bucket/grab :new :meta {:destination :db})
          old-id (:id old-bucket)
          result (spouts/pour-into new-bucket old-bucket :meta-merge-type :snapshot :pour-type :gather)]
      (is (= {:destination :db
              :previous-buckets [{old-id {}}]}
             (:meta result))
          "snapshot should create entry for old meta even when empty"))))

(deftest pour-into-meta-snapshot-appends-to-existing
  (testing "snapshot appends to existing :previous-buckets vector"
    (let [first-old-id "bucket-001"
          old-bucket (bucket/grab :old :meta {:source :api :version 1})
          new-bucket (bucket/grab :new :meta {:destination :db :previous-buckets [{first-old-id {:original :data}}]})
          old-id (:id old-bucket)
          result (spouts/pour-into new-bucket old-bucket :meta-merge-type :snapshot :pour-type :gather)]
      (is (= {:destination :db
              :previous-buckets [{first-old-id {:original :data}}
                                 {old-id {:source :api :version 1}}]}
             (:meta result))
          "snapshot should append to existing :previous-buckets vector"))))

(deftest pour-into-error-from-new-bucket
  (testing "error field comes from new bucket"
    (let [old-error (ex-info "old error" {})
          new-error (ex-info "new error" {})
          old-bucket (bucket/grab :old  :error [old-error "old msg"])
          new-bucket (bucket/grab :new  :error [new-error "new msg"])
          result (spouts/pour-into new-bucket old-bucket {:pour-type :gather})]
      (is (= [new-error "new msg"] (:error result))
          "error should come from new bucket"))))

(deftest pour-into-empty-logs
  (testing "handles buckets with no logs"
    (let [old-bucket (bucket/grab :old)
          new-bucket (bucket/grab :new)
          result (spouts/pour-into new-bucket old-bucket {:pour-type :gather})]
      (is (= [] (:logs result))
          "should handle empty logs gracefully"))))
