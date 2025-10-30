(ns bucket.combinators.gather-with-test
  "Bucket/gather-with - map a Bucket function over items, then gather results."
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(deftest gather-with-success
  (testing "double numbers"
    (let [times-two-bucket (fn [x] (bucket/grab (* x 2)))
          result (bucket/gather-with times-two-bucket [1 2 3 4 5])]
      (is (= {:id (:id result)
              :name (str (:id result) "-bucket")
              :meta {}
              :value [2 4 6 8 10]
              :error [nil nil]
              :logs []}
             result)
          "gather-with maps function over items and collects results")))

  (testing "with logging per item"
    (let [log-for (fn [s]
                    {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :debug :value (str "Do " s)})
          add-exclamation-bucket (fn [s] (bucket/grab (str s "!") :logs [(log-for s)]))
          result (bucket/gather-with add-exclamation-bucket ["a" "b"])]
      (is (= {:id (:id result)
              :name (str (:id result) "-bucket")
              :meta {}
              :value ["a!" "b!"]
              :error [nil nil]
              :logs [(log-for "a") (log-for "b")]}
             result)
          "gather-with accumulates logs from each mapped operation"))))

(deftest gather-with-errors
  (testing "stop on first error"
    (let [ensure-positive-bucket (fn [n] (if (pos? n)
                                           (bucket/grab n)
                                           (bucket/grab :error ["Must be positive"])))
          result (bucket/gather-with ensure-positive-bucket [1 2 0 3])]
      (is (= {:id (:id result)
              :name (str (:id result) "-bucket")
              :meta {}
              :value nil
              :error ["Must be positive"]
              :logs []}
             result)
          "gather-with fails fast on first error")))

  (testing "error preserves prior logs"
    (let [l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "done1"}
          with-log-or-error-bucket (fn [i] (if (= i 2)
                                             (bucket/grab :error ["err"])
                                             (bucket/grab i :logs [l])))
          result (bucket/gather-with with-log-or-error-bucket [1 2 3])]
      (is (= {:id (:id result)
              :name (str (:id result) "-bucket")
              :meta {}
              :value nil
              :error ["err"]
              :logs [l]}
             result)
          "gather-with preserves logs from operations before error"))))

(deftest gather-with-metadata-behavior
  (testing "gather-with creates new bucket with empty metadata"
    (let [times-two-with-meta (fn [x] (bucket/grab (* x 2) :meta {:computed true :value x}))
          result (bucket/gather-with times-two-with-meta [1 2 3])]
      (is (= [2 4 6]
             (:value result)))
      (is (= {}
             (:meta result))
          "gather-with creates new bucket with empty metadata, ignoring metadata from mapped buckets")))

  (testing "gather-with with complex metadata in each mapped bucket"
    (let [log-for (fn [s] {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :debug :value (str "Process " s)})
          process-with-meta (fn [s] (bucket/grab (str s "!") :logs [(log-for s)] :meta {:item s :stage :processed}))
          result (bucket/gather-with process-with-meta ["a" "b" "c"])]
      (is (= ["a!" "b!" "c!"]
             (:value result)))
      (is (= {}
             (:meta result))
          "metadata from each mapped bucket is not accumulated in final result"))))
