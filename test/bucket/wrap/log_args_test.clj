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
          [header arg-log] (:logs resp)]
      (is (= :ok (:value resp)))
      (is (= "args:" (:value header)))
      (is (= "arg: :foo, value: \"bar\"" (:value arg-log)))
      (is (= 0 (:indent header)))
      (is (= 0 (:indent arg-log))))))

(deftest log-args-redacts-passwords
  (testing "redacts password-like values by default"
    (let [f (wrap/log-args demo-fn)
          resp (f {:password "secret"})
          [_ arg-log] (:logs resp)]
      (is (= "* log redacted *" (:value arg-log))))))

(deftest log-args-no-redaction-when-disabled
  (testing "can disable password redaction"
    (let [f (wrap/log-args demo-fn :check-secrets false)
          resp (f {:password "secret"})
          [_ arg-log] (:logs resp)]
      (is (= "arg: :password, value: \"secret\"" (:value arg-log))))))
