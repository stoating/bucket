(ns bucket.combinators.pass-thread-test
  "Bucket/pass-> - composition of Bucket functions."
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(deftest pass-thread-basic-composition
  (testing "pass-> composes bucket functions sequentially"
    (let [add5 (fn [x] (bucket/grab (+ x 5)))
          mul2 (fn [x] (bucket/grab (* x 2)))
          pipeline (bucket/pass-> add5 mul2)
          input-val 3
          intermediate (add5 input-val)
          result (pipeline input-val)]
      (is (not= (:id intermediate) (:id result)))
      (is (= {:id (:id result)
              :name (str (:id result) "-bucket")
              :meta {}
              :result 16
              :error [nil nil]
              :logs []}
             result)
          "pass-> pipes result through composed functions with unique IDs"))))

(deftest pass-thread-log-accumulation
  (testing "pass-> accumulates logs from all functions"
    (let [l1 {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Step1"}
          l2 {:indent 0 :time (Instant/parse "2024-01-15T10:30:01Z") :level :info :value "Step2"}
          add-one-with-log (fn [x] (bucket/grab (+ x 1) :logs [l1]))
          triple-with-log (fn [x] (bucket/grab (* x 3) :logs [l2]))
          pipeline (bucket/pass-> add-one-with-log triple-with-log)
          input-val 2
          intermediate (add-one-with-log input-val)
          result (pipeline input-val)]
      (is (not= (:id intermediate) (:id result)))
      (is (= {:id (:id result)
              :name (str (:id result) "-bucket")
              :meta {}
              :result 9
              :error [nil nil]
              :logs [l1 l2]}
             result)
          "pass-> accumulates logs from each step in the pipeline"))))

(deftest pass-thread-error-short-circuit
  (testing "pass-> short-circuits on error"
    (let [fail (fn [_] (bucket/grab :error ["fail"]))
          next (fn [x] (bucket/grab (inc x)))
          pipeline (bucket/pass-> fail next)
          err (fail :any)
          result (pipeline :any)]
      (is (not= (:id err) (:id result)))
      (is (= {:id (:id result)
              :name (str (:id result) "-bucket")
              :meta {}
              :result nil
              :error ["fail"]
              :logs []}
             result)
          "pass-> stops at first error without executing remaining functions"))))

(deftest pass-thread-metadata-behavior
  (testing "pass-> preserves metadata from first function in pipeline"
    (let [add5-with-meta (fn [x] (bucket/grab (+ x 5) :meta {:step :add :value 5}))
          mul2-with-meta (fn [x] (bucket/grab (* x 2) :meta {:step :multiply :value 2}))
          pipeline (bucket/pass-> add5-with-meta mul2-with-meta)
          result (pipeline 3)]
      (is (= 16
             (:result result)))
      (is (= {:step :add :value 5}
             (:meta result))
          "pass-> preserves metadata from first function, ignoring subsequent functions' metadata")))

  (testing "pass-> metadata behavior with multiple steps"
    (let [l1 {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Step1"}
          l2 {:indent 0 :time (Instant/parse "2024-01-15T10:30:01Z") :level :info :value "Step2"}
          step1 (fn [x] (bucket/grab (+ x 1) :logs [l1] :meta {:stage :first :input x}))
          step2 (fn [x] (bucket/grab (* x 3) :logs [l2] :meta {:stage :second :factor 3}))
          pipeline (bucket/pass-> step1 step2)
          result (pipeline 2)]
      (is (= 9
             (:result result)))
      (is (= [l1 l2]
             (:logs result))
          "logs are accumulated")
      (is (= {:stage :first :input 2}
             (:meta result))
          "metadata from first function in pipeline is preserved, second function's metadata is ignored"))))
