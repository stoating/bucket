(ns bucket.spouts.serialize-bucket-test
  "Tests for bucket spouts serialize-bucket function."
  (:require [bucket :as bucket]
            [bucket.spouts.reserve :as spouts]
            [bucket.log.entry :as log-entry]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [jsonista.core :as json]
            [test-helpers :as th])
  (:import [java.time Instant]))

(use-fixtures :each th/clean-test-temp-fixture)

(deftest serialize-bucket-edn-format-test
  (testing "serializes to EDN format and prints to stdout"
    (let [logs [(log-entry/make "Test log" :info 0)]
          bucket (bucket/grab "test-result" :logs logs :meta {:key "value"})
          out-str (with-out-str
                    (let [serialized (spouts/serialize-bucket bucket :format :edn :out :stdout)]
                      (is (string? serialized))
                      (let [deserialized (edn/read-string serialized)]
                        (is (= (:id bucket) (:id deserialized)))
                        (is (= (:name bucket) (:name deserialized)))
                        (is (= (:result bucket) (:result deserialized)))
                        (is (= (:meta bucket) (:meta deserialized))))))]
      (is (.contains out-str "test-result")
          "serialize-bucket with EDN format produces valid EDN and prints to stdout"))))

(deftest serialize-bucket-edn-to-file-test
  (testing "serializes to EDN and writes to file"
    (let [bucket (bucket/grab {:data "value"} :name "test-bucket")
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (spouts/serialize-bucket bucket
                                             :format :edn
                                             :out :file
                                             :dir temp-dir
                                             :name "test"))]
      (is (.contains out-str "Bucket serialized to:"))
      (let [edn-files (filter #(.endsWith (.getName %) ".edn")
                              (.listFiles (io/file temp-dir)))]
        (is (= 1 (count edn-files)))
        (let [file-contents (slurp (first edn-files))
              deserialized (edn/read-string file-contents)]
          (is (= (:id bucket) (:id deserialized)))
          (is (= (:result bucket) (:result deserialized)))
          (is (.contains (.getName (first edn-files)) "test.edn")
              "serialize-bucket writes EDN to file with correct name"))))))

(deftest serialize-bucket-edn-to-both-test
  (testing "serializes to EDN and outputs to both stdout and file"
    (let [bucket (bucket/grab "both-output")
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (spouts/serialize-bucket bucket
                                             :format :edn
                                             :out :both
                                             :dir temp-dir
                                             :name "both"))]
      (is (.contains out-str "both-output"))
      (is (.contains out-str "Bucket serialized to:"))
      (let [edn-files (filter #(.endsWith (.getName %) ".edn")
                              (.listFiles (io/file temp-dir)))]
        (is (= 1 (count edn-files))
            "serialize-bucket with :both writes EDN to both stdout and file")))))

(deftest serialize-bucket-edn-none-output-test
  (testing "serializes to EDN without any output"
    (let [bucket (bucket/grab "no-output")
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (let [serialized (spouts/serialize-bucket bucket
                                                              :format :edn
                                                              :out :none
                                                              :dir temp-dir)]
                      (is (string? serialized))
                      (is (.contains serialized "no-output"))))]
      (is (empty? out-str))
      (is (zero? (count (.listFiles (io/file temp-dir))))
          "serialize-bucket with :none produces no output"))))

;; JSON Format Tests

(deftest serialize-bucket-json-format-test
  (testing "serializes to valid JSON format"
    (let [logs [(log-entry/make "JSON test" :info 0)]
          bucket (bucket/grab {:user "alice"} :logs logs :meta {:version 1})
          out-str (with-out-str
                    (let [serialized (spouts/serialize-bucket bucket :format :json :out :stdout)]
                      (is (string? serialized))
                      (let [parsed (json/read-value serialized)]
                        (is (map? parsed))
                        (is (contains? parsed "id"))
                        (is (contains? parsed "name"))
                        (is (contains? parsed "result"))
                        (is (contains? parsed "logs"))
                        (is (contains? parsed "meta"))
                        (is (contains? parsed "error"))
                        (is (= {"user" "alice"} (get parsed "result")))
                        (is (= {"version" 1} (get parsed "meta"))))))]
      (is (.contains out-str "alice")
          "serialize-bucket produces valid JSON"))))

(deftest serialize-bucket-json-with-error-test
  (testing "serializes bucket with error to JSON"
    (let [ex (ex-info "Test error" {:code 500})
          logs [(log-entry/make "Error occurred" :error 0)]
          bucket (bucket/grab "partial-result" :logs logs :error [ex "Error context"])
          serialized (spouts/serialize-bucket bucket :format :json :out :none)
          parsed (json/read-value serialized)]
      (is (map? (get parsed "error")))
      (is (string? (get-in parsed ["error" "exception"])))
      (is (.contains (get-in parsed ["error" "exception"]) "Test error"))
      (is (= "Error context" (get-in parsed ["error" "stacktrace"])))
      (is (= "partial-result" (get parsed "result"))
          "serialize-bucket handles errors in JSON format"))))

(deftest serialize-bucket-json-with-instant-test
  (testing "serializes bucket with Instant timestamps to JSON"
    (let [instant (Instant/parse "2024-01-15T10:30:00Z")
          logs [{:indent 0 :time instant :level :info :value "With instant"}]
          bucket (bucket/grab "data" :logs logs)
          serialized (spouts/serialize-bucket bucket :format :json :out :none)
          parsed (json/read-value serialized)]
      (is (vector? (get parsed "logs")))
      (is (string? (get-in parsed ["logs" 0 "time"])))
      (is (.contains (get-in parsed ["logs" 0 "time"]) "2024-01-15")
          "serialize-bucket converts Instant to string in JSON"))))

(deftest serialize-bucket-json-to-file-test
  (testing "serializes to JSON and writes to file"
    (let [bucket (bucket/grab [1 2 3] :name "json-bucket")
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (spouts/serialize-bucket bucket
                                             :format :json
                                             :out :file
                                             :dir temp-dir
                                             :name "test-json"))]
      (is (.contains out-str "Bucket serialized to:"))
      (let [json-files (filter #(.endsWith (.getName %) ".json")
                               (.listFiles (io/file temp-dir)))]
        (is (= 1 (count json-files)))
        (let [file-contents (slurp (first json-files))
              parsed (json/read-value file-contents)]
          (is (= [1 2 3] (get parsed "result")))
          (is (.contains (.getName (first json-files)) "test-json.json")
              "serialize-bucket writes JSON to file with correct extension"))))))

(deftest serialize-bucket-json-to-both-test
  (testing "serializes to JSON and outputs to both stdout and file"
    (let [bucket (bucket/grab {:status "ok"})
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (spouts/serialize-bucket bucket
                                             :format :json
                                             :out :both
                                             :dir temp-dir
                                             :name "json-both"))]
      (is (.contains out-str "\"status\""))
      (is (.contains out-str "Bucket serialized to:"))
      (let [json-files (filter #(.endsWith (.getName %) ".json")
                               (.listFiles (io/file temp-dir)))]
        (is (= 1 (count json-files))
            "serialize-bucket with :both writes JSON to both stdout and file")))))

;; Complex Data Tests

(deftest serialize-bucket-complex-result-test
  (testing "serializes bucket with complex nested result"
    (let [complex-result {:users [{:name "alice" :age 30}
                                  {:name "bob" :age 25}]
                          :metadata {:count 2 :tags ["admin" "user"]}}
          bucket (bucket/grab complex-result)
          json-serialized (spouts/serialize-bucket bucket :format :json :out :none)
          edn-serialized (spouts/serialize-bucket bucket :format :edn :out :none)
          json-parsed (json/read-value json-serialized)
          edn-parsed (edn/read-string edn-serialized)]
      ;; JSON uses string keys, so we need to compare with the stringified version
      (is (= {"users" [{"name" "alice" "age" 30}
                       {"name" "bob" "age" 25}]
              "metadata" {"count" 2 "tags" ["admin" "user"]}}
             (get json-parsed "result")))
      (is (= complex-result (:result edn-parsed))
          "serialize-bucket handles complex nested data structures"))))

(deftest serialize-bucket-with-nil-result-test
  (testing "serializes bucket with nil result"
    (let [bucket (bucket/grab nil :meta {:processed true})
          json-serialized (spouts/serialize-bucket bucket :format :json :out :none)
          edn-serialized (spouts/serialize-bucket bucket :format :edn :out :none)
          json-parsed (json/read-value json-serialized)
          edn-parsed (edn/read-string edn-serialized)]
      (is (nil? (get json-parsed "result")))
      (is (nil? (:result edn-parsed)))
      (is (= {"processed" true} (get json-parsed "meta")))
      (is (= {:processed true} (:meta edn-parsed))
          "serialize-bucket handles nil results correctly"))))

;; Default Behavior Tests

(deftest serialize-bucket-default-format-test
  (testing "defaults to EDN format and stdout output"
    (let [bucket (bucket/grab "default-test")
          out-str (with-out-str
                    (let [serialized (spouts/serialize-bucket bucket)]
                      (is (string? serialized))
                      (let [deserialized (edn/read-string serialized)]
                        (is (= (:result bucket) (:result deserialized))))))]
      (is (.contains out-str "default-test")
          "serialize-bucket defaults to EDN format and stdout output"))))

(deftest serialize-bucket-timestamp-in-filename-test
  (testing "includes timestamp in filename by default"
    (let [bucket (bucket/grab "timestamped")
          temp-dir th/test-temp-root
          _ (spouts/serialize-bucket bucket
                                     :format :edn
                                     :out :file
                                     :dir temp-dir
                                     :name "test")
          files (.listFiles (io/file temp-dir))]
      (is (= 1 (count files)))
      (is (re-find #"\d{15}-test\.edn" (.getName (first files)))
          "serialize-bucket includes timestamp in filename by default"))))

(deftest serialize-bucket-no-timestamp-in-filename-test
  (testing "excludes timestamp when timestamp is false"
    (let [bucket (bucket/grab "no-timestamp")
          temp-dir th/test-temp-root
          _ (spouts/serialize-bucket bucket
                                     :format :json
                                     :out :file
                                     :dir temp-dir
                                     :name "notimestamp"
                                     :timestamp? false)
          files (.listFiles (io/file temp-dir))]
      (is (= 1 (count files)))
      (is (= "notimestamp.json" (.getName (first files)))
          "serialize-bucket excludes timestamp when timestamp is false"))))

;; Error Handling Tests

(deftest serialize-bucket-unsupported-format-test
  (testing "throws on unsupported format"
    (let [bucket (bucket/grab "data")]
      (is (thrown-with-msg? Exception #"Unsupported format"
                            (spouts/serialize-bucket bucket :format :xml))
          "serialize-bucket throws exception for unsupported format"))))

(deftest serialize-bucket-roundtrip-edn-test
  (testing "EDN serialization roundtrip preserves bucket data"
    (let [logs [(log-entry/make "Log 1" :info 0)
                (log-entry/make "Log 2" :warning 4)]
          original (bucket/grab {:data "test"}
                                :logs logs
                                :meta {:version 1 :author "test"})
          serialized (spouts/serialize-bucket original :format :edn :out :none)
          deserialized (edn/read-string serialized)]
      (is (= (:id original) (:id deserialized)))
      (is (= (:name original) (:name deserialized)))
      (is (= (:result original) (:result deserialized)))
      (is (= (:logs original) (:logs deserialized)))
      (is (= (:meta original) (:meta deserialized)))
      (is (= (:error original) (:error deserialized))
          "EDN roundtrip preserves all bucket data"))))
