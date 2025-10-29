(ns bucket.spouts.spill-formatted-test
  "Tests for bucket spouts spill-formatted function."
  (:require [bucket :as bucket]
            [bucket.spouts :as spouts]
            [bucket.spouts.helpers.extract :as extract-spouts]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]
           [java.io StringWriter]))

(deftest spill-formatted-with-custom-formatters-test
  (testing "formats logs and errors with custom formatters"
    (let [logs [{:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Test log"}]
          ex (ex-info "Test error" {})
          bucket (bucket/grab "result" :logs logs :error [ex "context"])
          output (StringWriter.)
          log-formatter (fn [logs] (str/join "\n" (map #(str "LOG: " (:value %)) logs)))
          error-formatter (fn [[_ msg]] (str "ERROR: " msg))
          result (extract-spouts/spill-formatted bucket
                                         :out output
                                         :log-formatter log-formatter
                                         :error-formatter error-formatter
                                         :require-result false)]
      (is (= "result" result))
      (is (str/includes? (str output) "LOG: Test log"))
      (is (str/includes? (str output) "ERROR: context")
          "spill-formatted uses custom formatters for logs and errors"))))

(deftest spill-formatted-no-error-test
  (testing "handles bucket with no error"
    (let [logs [{:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Test"}]
          bucket (bucket/grab "data" :logs logs)
          output (StringWriter.)
          log-formatter (fn [logs] (str/join "\n" (map #(str "CUSTOM: " (:value %)) logs)))
          error-formatter (fn [[ex msg]]
                            (when (or ex msg)
                              "ERROR DETECTED"))
          result (extract-spouts/spill-formatted bucket
                                         :out output
                                         :log-formatter log-formatter
                                         :error-formatter error-formatter
                                         :require-result false)]
      (is (= "data" result))
      (is (str/includes? (str output) "CUSTOM: Test"))
      (is (not (str/includes? (str output) "ERROR DETECTED"))
          "spill-formatted calls error-formatter, but formatter returns nil when no error"))))

(deftest spill-formatted-json-style-test
  (testing "formats logs in JSON style"
    (let [logs [{:indent 1 :time 1000 :level :debug :value "Processing"}]
          bucket (bucket/grab {:status "ok"} :logs logs)
          output (StringWriter.)
          json-log-formatter (fn [logs]
                               (str/join "\n"
                                         (map #(str "{\"level\":\"" (name (:level %))
                                                    "\",\"message\":\"" (:value %) "\"}")
                                              logs)))
          json-error-formatter (fn [_] "{}")
          result (extract-spouts/spill-formatted bucket
                                         :out output
                                         :log-formatter json-log-formatter
                                         :error-formatter json-error-formatter
                                         :require-result false)]
      (is (= {:status "ok"} result))
      (is (str/includes? (str output) "{\"level\":\"debug\",\"message\":\"Processing\"}")
          "spill-formatted supports JSON-style custom formatters"))))

(deftest spill-formatted-with-side-effecting-formatter-test
  (testing "works with side-effecting formatters (no :out needed)"
    (let [logs [{:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "Test log"}]
          bucket (bucket/grab "result-value" :logs logs)
          output (atom [])
          log-formatter (fn [logs]
                          (doseq [log logs]
                            (swap! output conj (str "LOGGED: " (:value log))))
                          nil)
          result (extract-spouts/spill-formatted bucket
                                         :log-formatter log-formatter
                                         :require-result false)]
      (is (= "result-value" result))
      (is (= ["LOGGED: Test log"] @output)
          "side-effecting formatter is called and modifies external state"))))

(deftest spill-formatted-respects-require-result-test
  (testing "respects require-result parameter"
    (let [bucket (bucket/grab nil)
          output (StringWriter.)
          log-formatter identity
          error-formatter identity]
      (is (thrown? Exception
                   (extract-spouts/spill-formatted bucket
                                           :out output
                                           :log-formatter log-formatter
                                           :error-formatter error-formatter
                                           :require-result true))
          "spill-formatted throws exception when result is nil and require-result is true"))))
