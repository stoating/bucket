(ns bucket.wrap.catch-error-test
  (:require [bucket :as bucket]
            [bucket.wraps.catch-error :as wrap]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(defn ^{:name test-fn-ok}
  test-fn-ok
  [{:keys [logs]}]
  (bucket/grab :ok :logs (or logs [])))

(defn ^{:name test-fn-boom}
  test-fn-boom
  [_]
  (throw (ex-info "boom" {:k :v})))

(deftest catch-error-pass-through
  (testing "successful value unchanged"
    (let [l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "start"}
          f (wrap/catch-error test-fn-ok)
          resp (f {:logs [l]})]
      (is (= {:id (:id resp)
              :name (str (:id resp) "-bucket")
              :meta {}
              :value :ok
              :error [nil nil]
              :logs [l]}
             resp)
          "catch-error passes through successful values"))))

(deftest catch-error-catches-exception
  (testing "exception caught and wrapped"
    (let [l {:indent 1 :time (Instant/parse "2024-01-15T10:30:01Z") :level :debug :value "pre"}
          f (wrap/catch-error test-fn-boom)
          resp (f {:logs [l]})]
      (is (= nil (:value resp)))
      (is (= [l] (:logs resp)))
      (is (instance? Exception (first (:error resp))))
      (is (= "boom" (.getMessage ^Exception (first (:error resp))))
          "catch-error converts exceptions to error buckets preserving logs"))))

(deftest catch-error-default-logs
  (testing "empty logs when none provided"
    (let [f (wrap/catch-error test-fn-ok)
          resp (f {})]
      (is (= {:id (:id resp)
              :name (str (:id resp) "-bucket")
              :meta {}
              :value :ok
              :error [nil nil]
              :logs []}
             resp)
          "catch-error uses empty logs by default"))))
