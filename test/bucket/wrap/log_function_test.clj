(ns bucket.wrap.log-function-test
  (:require [bucket :as bucket]
            [bucket.wraps.log-function :as wrap]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(def demo-fn
  (with-meta
    (fn [{:keys [logs]}]
      (bucket/grab :ok :logs (or logs [])))
    {:name 'demo}))

(def task-fn
  (with-meta
    (fn [{:keys [logs]}]
      (bucket/grab :done :logs (or logs [])))
    {:name 'task}))

(deftest log-function-adds-entry-exit
  (testing "entry and exit logs added"
    (let [l0 {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "pre"}
          f (wrap/log-function demo-fn)
          resp (f {:logs [l0]})
          [lpre lenter lexit] (:logs resp)]
      (is (= :ok (:value resp)))
      (is (= l0 lpre))
      (is (= "--> demo" (:value lenter)))
      (is (= "<-- demo" (:value lexit)))
      (is (= 4 (:indent lenter)))
      (is (= 4 (:indent lexit)))
      (is (= 0 (:indent-next lexit))
          "log-function wraps function with entry/exit logging"))))

(deftest log-function-base-indent
  (testing "base indent from last log"
    (let [l0 {:indent 6 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "pre"}
          f (wrap/log-function task-fn)
          resp (f {:logs [l0]})
          [_ lenter lexit] (:logs resp)]
      (is (= :done (:value resp)))
      (is (= 10 (:indent lenter)))
      (is (= 10 (:indent lexit)))
      (is (= 6 (:indent-next lexit))
          "log-function uses existing log indent as base"))))

(deftest log-function-no-initial-logs
  (testing "indent starts at zero with no logs"
    (let [f (wrap/log-function demo-fn)
          resp (f {})
          [lenter lexit] (:logs resp)]
      (is (= :ok (:value resp)))
      (is (= 4 (:indent lenter)))
      (is (= 4 (:indent lexit)))
      (is (= 0 (:indent-next lexit))
          "log-function starts at indent 0 when no existing logs"))))

(deftest log-function-custom-spacing
  (testing "custom spacing overrides default indent width"
    (let [f (wrap/log-function demo-fn :spacing 2)
          resp (f {})
          [lenter lexit] (:logs resp)]
      (is (= :ok (:value resp)))
      (is (= 2 (:indent lenter)))
      (is (= "<-- demo" (:value lexit)))
      (is (= 2 (:indent lexit)))
      (is (= 0 (:indent-next lexit))
          "custom spacing still resets exit indentation to base indent"))))