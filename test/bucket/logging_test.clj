(ns bucket.logging-test
  (:require [bin.format :as format]
            [bucket :as bucket]
            [bucket.log :as log]
            [bucket.log.secret :as secret]
            [bucket.meta :as meta]
            [bucket.log.temp :as temp]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is are use-fixtures]]
            [test-helpers :as th])
  (:import [java.time Instant]))

(use-fixtures :each th/clean-test-temp-fixture)

(deftest make-entry-test
  (testing "make-entry with different arities"
    (testing "single argument"
      (let [entry (temp/make-entry "test message")]
        (is (= "test message" (:value entry)))
        (is (= :info (:level entry)))
        (is (= 0 (:indent entry)))
        (is (instance? Long (:time entry))
            "make-entry creates entry with message, default :info level, 0 indent, and timestamp")))

    (testing "message and level"
      (let [entry (temp/make-entry "test message" :level :error)]
        (is (= "test message" (:value entry)))
        (is (= :error (:level entry)))
        (is (= 0 (:indent entry)))
        (is (instance? Long (:time entry))
            "make-entry creates entry with message, specified level, 0 indent, and timestamp")))

    (testing "message, level, and indent"
      (let [entry (temp/make-entry "test message" :level :warning :indent 3)]
        (is (= "test message" (:value entry)))
        (is (= :warning (:level entry)))
        (is (= 3 (:indent entry)))
        (is (instance? Long (:time entry))
            "make-entry creates entry with message, level, indent, and timestamp")))))

(deftest likely-secret?-test
  (testing "exact matches"
    (are [input expected] (= expected (secret/likely-secret? input))
      "confidential" true
      "Confidential" true
      "CONFIDENTIAL" true
      "encrypted" true
      "hash" true
      "nonconfidential" false ; should not match as substring
      "confidence" false)) ; should not match as substring

  (testing "contains patterns"
    (are [input expected] (= expected (secret/likely-secret? input))
      ;; api
      "apikey" true
      "my_api_endpoint" true
      "API_GATEWAY" true

      ;; auth
      "auth_token" true
      "authorization" true
      "authenticate" true

      ;; cert
      "certificate" true
      "cert_file" true
      "SSL_CERT" true

      ;; cipher
      "cipher_text" true
      "ciphertext" true
      "decipher" true

      ;; cred
      "credentials" true
      "cred_store" true
      "my_creds" true

      ;; key
      "apikey" true
      "secret_key" true
      "keystore" true
      "KEY_VALUE" true

      ;; pass
      "password" true
      "pass123" true
      "bypass" true
      "passport" true

      ;; pw - should match only as whole word
      "pw" true
      "my pw" true
      "PW" true
      "pw-123" true ; pw is at word boundary (hyphen breaks word)
      "pwned" true ; 'pw' not at word boundary
      "pw_123" true

      ;; secret
      "secret_token" true
      "my_secret" true
      "SECRET_KEY" true
      "secretary" true ; contains 'secret'

      ;; sha
      "sha256" true
      "sha1_hash" true
      "SHA512" true

      ;; signature
      "signature_key" true
      "digital_signature" true
      "SIGNATURE" true

      ;; token
      "auth_token" true
      "bearer_token" true
      "TOKEN_VALUE" true))

  (testing "word boundary patterns - only match as complete words"
    (are [input expected] (= expected (secret/likely-secret? input))
      ;; pin
      "pin_code" true
      "spinning" false
      "PIN_NUMBER" true

      ;; sec - should match only as whole word
      "sec" true
      "SEC" true
      "my sec key" true
      "sec-value" true ; sec is at word boundary (hyphen breaks word)
      "second" false ; 'sec' not at word boundary
      "insecure" false ; 'sec' not at word boundary
      "section" false ; 'sec' not at word boundary
      "sec_value" true

      ;; sig - should match only as whole word
      "sig" true
      "SIG" true
      "my sig" true
      "sig-value" true ; sig is at word boundary (hyphen breaks word)
      "signal" false ; 'sig' not at word boundary
      "design" false)) ; 'sig' not at word boundary ; 'sig' not at word boundary (also caught by contains)

  (testing "compound patterns"
    (are [input expected] (= expected (secret/likely-secret? input))
      ;; api.*key patterns
      "apikey" true
      "api_key" true
      "api-key" true
      "API_KEY" true
      "api.key" true
      "api key" true

      ;; api.*val patterns
      "apival" true
      "api_val" true
      "api-val" true
      "API_VALUE" true
      "api.value" true

      ;; key.*val patterns
      "keyval" true
      "key_val" true
      "key-val" true
      "KEY_VALUE" true
      "key.value" true
      "key = val" true))

  (testing "URL with credentials"
    (are [input expected] (= expected (secret/likely-secret? input))
      "https://user:password@example.com" true
      "http://admin:secret@localhost:8080" true
      "https://user:pass123@api.example.com/path" true
      "https://example.com" false
      "http://example.com/path" false
      "user@example.com" false))

  (testing "safe strings that should not be detected"
    (are [input expected] (= expected (secret/likely-secret? input))
      "regular message" false
      "user@example.com" false
      "this is just text" false
      "processing data" false
      "hello world" false
      "count: 42" false
      "status: success" false
      ;; Word boundary edge cases - these should NOT match
      "design" false ; contains 'sig' but not at word boundary
      "second" false ; contains 'sec' but not at word boundary
      "insecure" false ; contains 'sec' but not at word boundary
      "signal" false))) ; contains 'sig' but not at word boundary

(deftest log-test
  (testing "log function with different arities"
    (let [initial-logs []]
      (testing "basic logging"
        (let [logs (log/log initial-logs "test message")]
          (is (= 1 (count logs)))
          (let [entry (first logs)]
            (is (= "test message" (:value entry)))
            (is (= :info (:level entry)))
            (is (= 0 (:indent entry))
                "log adds entry with message, default :info level, and 0 indent"))))

      (testing "logging with level"
        (let [logs (log/log initial-logs "error message" :level :error)]
          (is (= 1 (count logs)))
          (let [entry (first logs)]
            (is (= "error message" (:value entry)))
            (is (= :error (:level entry)))
            (is (= 0 (:indent entry))
                "log adds entry with message, specified level, and 0 indent"))))

      (testing "logging with level and indent"
        (let [logs (log/log initial-logs "indented message" :level :warning :indent 2)]
          (is (= 1 (count logs)))
          (let [entry (first logs)]
            (is (= "indented message" (:value entry)))
            (is (= :warning (:level entry)))
            (is (= 2 (:indent entry))
                "log adds entry with message, level, and indent"))))

      (testing "indent inheritance"
        (let [logs (-> initial-logs
                       (log/log "first message" :level :info :indent 3)
                       (log/log "second message"))]
          (is (= 2 (count logs)))
          (is (= 3 (:indent (first logs))))
          (is (= 3 (:indent (second logs)))
              "log inherits indent from previous entry")))

      (testing "password redaction"
        (let [logs (log/log initial-logs "password=secret123" :check-secrets true)]
          (is (= 1 (count logs)))
          (let [entry (first logs)]
            (is (= "* log redacted *" (:value entry))
                "log redacts password-like content when redaction enabled"))))

      (testing "logging into a bucket sink"
        (let [initial-bucket (bucket/grab :value {:status :ok})
              updated-bucket (log/log initial-bucket "bucket message" :level :warning)
              logs (:logs updated-bucket)]
          (is (map? updated-bucket))
          (is (= {:status :ok} (:result updated-bucket)))
          (is (= 1 (count logs)))
          (is (= :warning (:level (first logs))))
          (is (= "bucket message" (:value (first logs)))
              "log appends entries to a bucket and returns the updated bucket"))))))

(deftest format-log-message-test
  (testing "log message formatting"
    (let [test-time (Instant/parse "2023-12-01T10:30:45Z")
          entry {:indent 2 :time test-time :level :info :value "test message"}
          formatted (format/log-text entry)]
      (is (str/includes? formatted "INFO"))
      (is (str/includes? formatted "test message"))
      (is (str/includes? formatted "  "))
      (is (str/includes? formatted "-")
          "formatted message includes level, message text, indent spaces, and separators"))))

(deftest print-logs-test
  (testing "printing logs to output stream"
    (let [logs [(temp/make-entry "first message")
                (temp/make-entry "second message" :level :error :indent 1)]
          result (with-out-str
                   (log/print-logs logs :out :stdout))]
      (is (str/includes? result "first message"))
      (is (str/includes? result "second message"))
      (is (str/includes? result "INFO"))
      (is (str/includes? result "ERROR")
          "print-logs outputs all log entries with their levels to stdout")))

  (testing "printing logs from a bucket sink"
    (let [bucket (-> (bucket/grab :value {:status :ok})
                     (log/log "bucket message" :level :warning)
                     (log/log "second message"))
          result (with-out-str
                   (log/print-logs bucket :out :stdout))]
      (is (str/includes? result "bucket message"))
      (is (str/includes? result "second message"))
      (is (str/includes? result "WARNING"))
      (is (str/includes? result "INFO")
          "print-logs accepts a bucket sink and prints its logs with levels"))))

(deftest filter-test
  (let [base-time (Instant/parse "2024-01-01T00:00:00Z")
        logs [(-> (temp/make-entry "debug msg" :level :debug :indent 1)
                  (assoc :time (inst-ms base-time)))
              (-> (temp/make-entry "info msg" :level :info :indent 3)
                  (assoc :time (inst-ms (.plusSeconds base-time 60))))
              (-> (temp/make-entry "warning msg" :level :warning :indent 5)
                  (assoc :time (inst-ms (.plusSeconds base-time 120))))
              (-> (temp/make-entry "error msg" :level :error :indent 2)
                  (assoc :time (inst-ms (.plusSeconds base-time 180))))
              (-> (temp/make-entry "critical error" :level :critical :indent 6)
                  (assoc :time (inst-ms (.plusSeconds base-time 240))))]
        bucket (assoc (bucket/grab :value {:status :ok}) :logs logs)]
    (testing "default debug-only filtering"
      (let [filtered (log/filter logs)]
        (is (= 1 (count filtered)))
        (is (= :debug (:level (first filtered))))
        (is (= "debug msg" (:value (first filtered))))))

    (testing "level filtering with :gte"
      (let [filtered (log/filter logs :type :gte :value :warning)]
        (is (= 3 (count filtered)))
        (is (= [:warning :error :critical] (map :level filtered)))))

    (testing "level filtering with :eq"
      (let [filtered (log/filter logs :type :eq :value :info)]
        (is (= 1 (count filtered)))
        (is (= :info (:level (first filtered))))))

    (testing "indent filtering"
      (let [filtered (log/filter logs :mode :indent :type :gte :value 4)]
        (is (= 2 (count filtered)))
        (is (= [:warning :critical] (map :level filtered))))
      (let [filtered (log/filter logs :mode :indent :type :lte :value 2)]
        (is (= 2 (count filtered)))
        (is (= [:debug :error] (map :level filtered)))))

    (testing "time filtering with millis value"
      (let [cutoff-ms (inst-ms (.plusSeconds base-time 120))
            filtered (log/filter logs :mode :time :type :gte :value cutoff-ms)]
        (is (= 3 (count filtered)))
        (is (= [:warning :error :critical] (map :level filtered)))))

    (testing "time filtering with Instant value"
      (let [cutoff-instant (.plusSeconds base-time 120)
            filtered (log/filter logs :mode :time :type :gte :value cutoff-instant)]
        (is (= 3 (count filtered)))
        (is (= [:warning :error :critical] (map :level filtered)))))

    (testing "value filtering with regex"
      (let [filtered (log/filter logs :mode :value :type :eq :value #"(?i:error)")]
        (is (= 2 (count filtered)))
        (is (= [:error :critical] (map :level filtered))))
      (let [filtered (log/filter logs :mode :value :type :neq :value #"(?i:error)")]
        (is (= 3 (count filtered)))
        (is (= [:debug :info :warning] (map :level filtered)))))

    (testing "bucket sink returns bucket"
      (let [filtered-bucket (log/filter bucket :type :gte :value :info)
            filtered-logs (:logs filtered-bucket)]
        (is (map? filtered-bucket))
        (is (= {:status :ok} (:result filtered-bucket))
            "filtering preserves non-log bucket data")
        (is (= [:info :warning :error :critical] (map :level filtered-logs)))))))

(deftest convenience-functions-test
  (testing "debug function"
    (let [logs (log/debug [] "debug message")]
      (is (= 1 (count logs)))
      (is (= :debug (:level (first logs))))
      (is (= "debug message" (:value (first logs)))))

    (let [logs (log/debug [] "debug message" :indent 2)]
      (is (= 2 (:indent (first logs)))
          "debug creates log entry with :debug level and accepts indent parameter")))

  (testing "info function"
    (let [logs (log/info [] "info message")]
      (is (= 1 (count logs)))
      (is (= :info (:level (first logs))))
      (is (= "info message" (:value (first logs)))
          "info creates log entry with :info level")))

  (testing "warning function"
    (let [logs (log/warning [] "warning message")]
      (is (= 1 (count logs)))
      (is (= :warning (:level (first logs))))
      (is (= "warning message" (:value (first logs)))
          "warning creates log entry with :warning level")))

  (testing "error function"
    (let [logs (log/error [] "error message")]
      (is (= 1 (count logs)))
      (is (= :error (:level (first logs))))
      (is (= "error message" (:value (first logs)))
          "error creates log entry with :error level")))

  (testing "critical function"
    (let [logs (log/critical [] "critical message")]
      (is (= 1 (count logs)))
      (is (= :critical (:level (first logs))))
      (is (= "critical message" (:value (first logs)))
          "critical creates log entry with :critical level"))))

(deftest cli-integration-test
  (testing "logging behavior in CLI context"
    (testing "log accumulation across multiple operations"
      (let [logs (-> []
                     (log/info "Starting operation")
                     (log/debug "Processing step 1" :indent 2)
                     (log/debug "Processing step 2" :indent 2)
                     (log/warning "Minor issue detected" :indent 2)
                     (log/info "Operation completed"))]
        (is (= 5 (count logs)))
        (is (= :info (:level (first logs))))
        (is (= :info (:level (last logs))))
        (is (= 2 (:indent (nth logs 1))))
        (is (= 2 (:indent (nth logs 2))))
        (is (= 2 (:indent (nth logs 3)))
            "logs accumulate across operations with correct levels and indents")))

    (testing "log filtering for different verbosity levels"
      (let [logs [(temp/make-entry "debug info" :level :debug)
                  (temp/make-entry "general info" :level :info)
                  (temp/make-entry "warning" :level :warning)
                  (temp/make-entry "error occurred" :level :error)]
            info-and-above (log/filter logs :type :gte :value :info)
            warning-and-above (log/filter logs :type :gte :value :warning)]
        (is (= 3 (count info-and-above)))
        (is (= 2 (count warning-and-above))
            "filter enables different verbosity levels")))

    (testing "password redaction in CLI logging"
      (let [logs (-> []
                     (log/log "Starting authentication")
                     (log/log "api_key=secret123" :check-secrets true)
                     (log/log "Authentication successful"))]
        (is (= 3 (count logs)))
        (is (= "* log redacted *" (:value (second logs)))
            "password redaction works in CLI logging workflow")))))

(deftest log-output-format-test
  (testing "CLI-compatible log output format"
    (let [entry (temp/make-entry "CLI operation completed")
          printed-output (with-out-str
                           (log/print-logs [entry] :out :stdout))]
      (is (str/includes? printed-output "INFO"))
      (is (str/includes? printed-output "CLI operation completed"))
      (is (str/includes? printed-output "\n")
          "printed log output includes level, message, and newline"))))

(deftest log-file-output-test
  (testing "print-logs writes to file with :out :file"
    (let [logs [(temp/make-entry "First log message")
                (temp/make-entry "Second log message" :level :error :indent 1)]
          test-dir (str th/test-temp-root "/logs")]
      (log/print-logs logs :out :file :dir test-dir :timestamp false :name "test")
      (let [file (io/file (str test-dir "/test.log"))
            content (slurp file)]
        (is (.exists file))
        (is (str/includes? content "INFO"))
        (is (str/includes? content "First log message"))
        (is (str/includes? content "ERROR"))
        (is (str/includes? content "Second log message")
            "print-logs writes all log entries with levels to file"))))

  (testing "print-logs writes to file with timestamp"
    (let [logs [(temp/make-entry "Timestamped log")]
          test-dir (str th/test-temp-root "/logs")]
      (log/print-logs logs :out :file :dir test-dir :timestamp true)
      (let [files (.listFiles (io/file test-dir))]
        (is (pos? (count files)))
        (is (str/ends-with? (.getName (first files)) ".log")
            "print-logs creates timestamped log files"))))

  (testing "print-logs with :out :both writes to file and stdout"
    (let [logs [(temp/make-entry "Both output log" :level :warning)]
          test-dir (str th/test-temp-root "/logs")
          stdout-output (with-out-str
                          (log/print-logs logs :out :both :dir test-dir :timestamp false :name "both-test"))]
      (is (str/includes? stdout-output "WARNING"))
      (is (str/includes? stdout-output "Both output log"))
      (let [file (io/file (str test-dir "/both-test.log"))
            content (slurp file)]
        (is (.exists file))
        (is (str/includes? content "Both output log")
            "print-logs with :out :both writes to both stdout and file"))))

  (testing "print-meta writes to file"
    (let [meta {:bucket-name "test-bucket" :total-items 42}
          test-dir (str th/test-temp-root "/meta")]
      (meta/print-meta meta :out :file :dir test-dir :timestamp false :name "test-meta")
      (let [file (io/file (str test-dir "/test-meta.edn"))
            content (slurp file)]
        (is (.exists file))
        (is (str/includes? content ":bucket-name"))
        (is (str/includes? content "test-bucket"))
        (is (str/includes? content ":total-items"))
        (is (str/includes? content "42")
            "print-meta writes metadata to edn file"))))

  (testing "print-meta with :out :both writes to file and stdout"
    (let [meta {:test-key "test-value"}
          test-dir (str th/test-temp-root "/meta")
          stdout-output (with-out-str
                          (meta/print-meta meta :out :both :dir test-dir :timestamp false :name "both-meta"))]
      (is (str/includes? stdout-output ":test-key"))
      (is (str/includes? stdout-output "test-value"))
      (let [file (io/file (str test-dir "/both-meta.edn"))]
        (is (.exists file)
            "print-meta with :out :both writes to both stdout and file")))))
