(ns bucket.wrap.log-args-test
  (:require [bucket :as bucket]
            [bucket.wraps.log-args :as wrap]
            [clojure.test :refer [deftest is testing]]))

(def demo-fn
  (with-meta
    (fn [{:keys [logs]}]
      (bucket/grab :ok :logs (or logs [])))
    {:name 'demo}))

(deftest log-args-adds-log
  (testing "arguments logged before function execution"
    (let [f (wrap/log-args demo-fn)
          resp (f {:foo "bar"})
          [args-log] (:logs resp)]
      (is (= :ok (:result resp)))
      (is (= "args: {:foo \"bar\"}" (:value args-log)))
      (is (= 0 (:indent args-log))))))

(deftest log-args-redacts-passwords
  (testing "redacts password-like values by default"
    (let [f (wrap/log-args demo-fn)
          resp (f {:password "secret"})
          [args-log] (:logs resp)]
      (is (= "* log redacted *" (:value args-log))))))

(deftest log-args-no-redaction-when-disabled
  (testing "can disable password redaction"
    (let [f (wrap/log-args demo-fn :check-pass false)
          resp (f {:password "secret"})
          [args-log] (:logs resp)]
      (is (= "args: {:password \"secret\"}" (:value args-log))))))
