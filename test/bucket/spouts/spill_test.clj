(ns bucket.spouts.spill-test
  "Tests for bucket spouts spill function."
  (:require [bucket :as bucket]
            [bucket.log.entry :as log-entry]
            [bucket.spouts :as spouts]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [test-helpers :as th]))

(use-fixtures :each th/clean-test-temp-fixture)

(deftest spill-logs-to-stdout-only-test
  (testing "spill outputs logs to stdout only"
    (let [logs [(log-entry/make "First message")
                (log-entry/make "Second message" :level :debug :indent 4)]
          bucket (bucket/grab "value" :logs logs)
          out-before (with-out-str
                       (let [value (spouts/spill bucket :log-out :stdout :meta-out :none :out-dir th/test-temp-root)]
                         (is (= "value" value))))]
      (is (.contains out-before "First message"))
      (is (.contains out-before "Second message"))
      (is (.contains out-before "INFO"))
      (is (.contains out-before "DEBUG")
          "spill with :log-out :stdout prints all logs to stdout and returns value"))))

(deftest spill-logs-to-file-only-test
  (testing "spill outputs logs to file only"
    (let [logs [(log-entry/make "Log to file")
                (log-entry/make "Another log entry" :level :warning :indent 4)]
          bucket (bucket/grab "file-value" :logs logs)
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :file :bucket-out :none :meta-out :none :out-dir temp-dir)]
                      (is (= "file-value" value))))]
      (is (not (.contains out-str "Log to file")))
      (let [log-files (filter #(.endsWith (.getName %) ".log")
                              (.listFiles (io/file temp-dir)))]
        (is (= 1 (count log-files)))
        (let [file-contents (slurp (first log-files))]
          (is (.contains file-contents "Log to file"))
          (is (.contains file-contents "Another log entry"))
          (is (.contains file-contents "INFO"))
          (is (.contains file-contents "WARNING")
              "spill with :log-out :file writes logs to file and not stdout"))))))

(deftest spill-logs-to-both-stdout-and-file-test
  (testing "spill outputs logs to both stdout and file"
    (let [logs [(log-entry/make "First log")
                (log-entry/make "Second log" :level :debug :indent 4)]
          bucket (bucket/grab "both-value" :logs logs)
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :both :meta-out :none :out-dir temp-dir)]
                      (is (= "both-value" value))))]
      (is (.contains out-str "First log"))
      (is (.contains out-str "Second log"))
      (let [log-files (filter #(.endsWith (.getName %) ".log")
                              (.listFiles (io/file temp-dir)))]
        (is (= 1 (count log-files)))
        (let [file-contents (slurp (first log-files))]
          (is (.contains file-contents "First log"))
          (is (.contains file-contents "Second log"))
          (is (.contains file-contents "INFO"))
          (is (.contains file-contents "DEBUG")
              "spill with :log-out :both writes logs to both stdout and file"))))))

(deftest spill-logs-none-test
  (testing "spill with log-out :none produces no log output"
    (let [logs [(log-entry/make "Should not appear")]
          bucket (bucket/grab "data" :logs logs)
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :none :bucket-out :none :meta-out :none :out-dir temp-dir)]
                      (is (= "data" value))))]
      (is (not (.contains out-str "Should not appear")))
      (is (zero? (count (.listFiles (io/file temp-dir))))
          "spill with :log-out :none produces no output and creates no files"))))

(deftest spill-with-error-test
  (testing "spill handles error in bucket"
    (let [logs [(log-entry/make "Processing...")]
          ex (ex-info "Something went wrong" {:detail "bad data"})
          bucket (bucket/grab "partial-value" :logs logs :error [ex "Error context"])
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :stdout :meta-out :none :out-dir temp-dir :exit :continue)]
                      (is (= "partial-value" value))))]
      (is (.contains out-str "Processing..."))
      (is (.contains out-str "Something went wrong"))
      (is (.contains out-str "Error context")
          "spill outputs error information along with logs"))))

(deftest spill-meta-to-stdout-only-test
  (testing "spill outputs meta to stdout only"
    (let [logs [(log-entry/make "With meta")]
          bucket (bucket/grab "value" :logs logs :meta {:user "alice" :operation "test"})
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :none :meta-out :stdout :out-dir th/test-temp-root)]
                      (is (= "value" value))))]
      (is (.contains out-str "=== Bucket Metadata ==="))
      (is (.contains out-str ":user"))
      (is (.contains out-str "alice"))
      (is (.contains out-str ":operation"))
      (is (.contains out-str "test"))
      (is (not (.contains out-str "Metadata written to:"))
          "spill with :meta-out :stdout prints metadata to stdout only"))))

(deftest spill-meta-to-file-only-test
  (testing "spill writes meta to file only (not stdout)"
    (let [logs [(log-entry/make "File meta test" :info 0)]
          bucket (bucket/grab "value" :logs logs :meta {:env "prod" :version "1.0"} :name "file-test-bucket")
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :none :bucket-out :none :meta-out :file :out-dir temp-dir)]
                      (is (= "value" value))))]
      (is (not (.contains out-str "=== Bucket Metadata ===")))
      (is (not (.contains out-str ":env")))
      (is (.contains out-str "Metadata written to:"))
      (is (.contains out-str "file-test-bucket-meta.edn"))
      (is (= 1 (count (.listFiles (io/file temp-dir))))
          "spill with :meta-out :file writes metadata to file and not stdout"))))

(deftest spill-meta-to-both-stdout-and-file-test
  (testing "spill outputs meta to both stdout and file"
    (let [logs [(log-entry/make "Both outputs" :info 0)]
          bucket (bucket/grab "value" :logs logs :meta {:service "api" :request-id "123"} :name "both-output-bucket")
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :none :bucket-out :none :meta-out :both :out-dir temp-dir)]
                      (is (= "value" value))))]
      (is (.contains out-str "=== Bucket Metadata ==="))
      (is (.contains out-str ":service"))
      (is (.contains out-str "api"))
      (is (.contains out-str ":request-id"))
      (is (.contains out-str "123"))
      (is (.contains out-str "Metadata written to:"))
      (is (.contains out-str "both-output-bucket-meta.edn"))
      (is (= 1 (count (.listFiles (io/file temp-dir))))
          "spill with :meta-out :both writes metadata to both stdout and file"))))

(deftest spill-meta-none-test
  (testing "spill with meta-out :none produces no meta output"
    (let [logs [(log-entry/make "Default behavior" :info 0)]
          bucket (bucket/grab "value" :logs logs :meta {:should-not-appear "in output"})
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :none :bucket-out :none :meta-out :none :out-dir temp-dir)]
                      (is (= "value" value))))]
      (is (not (.contains out-str "=== Bucket Metadata ===")))
      (is (not (.contains out-str "should-not-appear")))
      (is (not (.contains out-str "Metadata written to:")))
      (is (zero? (count (.listFiles (io/file temp-dir))))
          "spill with :meta-out :none produces no metadata output or files"))))

(deftest spill-empty-meta-test
  (testing "spill handles empty meta gracefully"
    (let [logs [(log-entry/make "Empty meta" :info 0)]
          bucket (bucket/grab "value" :logs logs :meta {})
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :none :bucket-out :none :meta-out :stdout :out-dir th/test-temp-root)]
                      (is (= "value" value))))]
      (is (.contains out-str "=== Bucket Metadata ===")
          "spill prints metadata header even when metadata is empty"))))

(deftest spill-complex-meta-test
  (testing "spill handles complex nested metadata"
    (let [logs [(log-entry/make "Complex meta" :info 0)]
          bucket (bucket/grab "value" :logs logs :meta {:user {:name "bob" :id 42}
                                                        :context {:env "staging" :region "us-west"}
                                                        :tags ["critical" "monitored"]})
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :none :bucket-out :none :meta-out :stdout :out-dir th/test-temp-root)]
                      (is (= "value" value))))]
      (is (.contains out-str ":user"))
      (is (.contains out-str ":name"))
      (is (.contains out-str "bob"))
      (is (.contains out-str ":context"))
      (is (.contains out-str "staging"))
      (is (.contains out-str ":tags"))
      (is (.contains out-str "critical")
          "spill prints nested metadata structures correctly"))))

(deftest spill-logs-and-meta-to-stdout-test
  (testing "spill outputs both logs and meta to stdout"
    (let [logs [(log-entry/make "Log entry 1" :info 0)
                (log-entry/make "Log entry 2" :debug 4)]
          bucket (bucket/grab "combined-value" :logs logs :meta {:combined "test" :count 2})
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :stdout :bucket-out :none :meta-out :stdout :out-dir temp-dir)]
                      (is (= "combined-value" value))))]
      (is (.contains out-str "Log entry 1"))
      (is (.contains out-str "Log entry 2"))
      (is (.contains out-str "=== Bucket Metadata ==="))
      (is (.contains out-str ":combined"))
      (is (.contains out-str "test"))
      (is (.contains out-str ":count")
          "spill with both outputs to stdout prints both logs and metadata"))))

(deftest spill-everything-test
  (testing "spill with logs, error, and meta all present"
    (let [logs [(log-entry/make "Starting" :info 0)
                (log-entry/make "Processing" :info 4)
                (log-entry/make "Warning occurred" :warning 4)]
          ex (ex-info "Partial failure" {:code 500})
          bucket (bucket/grab "partial-data"
                              :logs logs
                              :error [ex "Partial error"]
                              :meta {:attempt 3 :max-retries 5}
                              :name "everything-bucket")
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :both :bucket-out :none :meta-out :both :out-dir temp-dir :exit :continue)]
                      (is (= "partial-data" value))))]
      (is (.contains out-str "Starting"))
      (is (.contains out-str "Processing"))
      (is (.contains out-str "Warning occurred"))
      (is (.contains out-str "Partial failure"))
      (is (.contains out-str "Partial error"))
      (is (.contains out-str "=== Bucket Metadata ==="))
      (is (.contains out-str ":attempt"))
      (is (.contains out-str "3"))
      (is (.contains out-str "Metadata written to:"))
      (is (.contains out-str "everything-bucket-meta.edn"))
      (is (= 3 (count (.listFiles (io/file temp-dir))))
          "spill with all outputs prints logs, error, metadata to both stdout and files"))))

(deftest spill-require-value-default-test
  (testing "spill returns nil when value is nil by default"
    (let [logs [(log-entry/make "Returning nil" :info 0)]
          bucket (bucket/grab nil :logs logs)
          out-str (with-out-str
                    (let [value (spouts/spill bucket :log-out :stdout :bucket-out :none :meta-out :none :out-dir th/test-temp-root)]
                      (is (nil? value))))]
      (is (.contains out-str "Returning nil")
          "spill returns nil without throwing by default"))))

(deftest spill-require-value-true-test
  (testing "spill throws exception when value is nil and require-value is true"
    (let [bucket (bucket/grab nil)]
      (is (thrown-with-msg? Exception #"value is nil"
                            (spouts/spill bucket :log-out :none :meta-out :none :require-value true :out-dir th/test-temp-root))))))

(deftest spill-require-value-false-test
  (testing "spill returns nil when value is nil and require-value is false"
    (let [logs [(log-entry/make "Returning nil" :info 0)]
          bucket (bucket/grab nil :logs logs)
          out-str (with-out-str
                    (let [value (spouts/spill bucket {:log-out :stdout
                                                      :meta-out :none
                                                      :require-value false
                                                      :out-dir th/test-temp-root})]
                      (is (nil? value))))]
      (is (.contains out-str "Returning nil")
          "spill returns nil without throwing when :require-value is false"))))

(deftest spill-default-outputs-both-test
  (testing "spill defaults to outputting both logs and meta to both stdout and file"
    (let [logs [(log-entry/make "Default test" :info 0)]
          bucket (bucket/grab "value" :logs logs :meta {:test "default"})
          temp-dir th/test-temp-root
          out-str (with-out-str
                    (spouts/spill bucket :bucket-out :none :out-dir temp-dir))]
      (is (.contains out-str "Default test"))
      (is (.contains out-str "=== Bucket Metadata ==="))
      (is (.contains out-str ":test"))
      (is (.contains out-str "Metadata written to:"))
      (is (= 2 (count (.listFiles (io/file temp-dir))))
          "spill with no output options defaults to both logs and metadata to both stdout and file"))))
