(ns bucket.error-test
  (:require [bin.format :as format]
            [bucket :as bucket]
            [bucket.error :as error]
            [bucket.error.entry :as error-entry]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [test-helpers :as th]))

(use-fixtures :each th/clean-test-temp-fixture)

(deftest make-with-exception-test
  (testing "make with exception generates stacktrace"
    (let [ex (RuntimeException. "Test error")
          [err stacktrace] (error-entry/make ex)]
      (is (= ex err))
      (is (string? stacktrace))
      (is (.contains stacktrace "RuntimeException"))
      (is (.contains stacktrace "Test error")
          "make returns exception with generated stacktrace string")))

  (testing "make with nil returns [nil nil]"
    (let [result (error-entry/make nil)]
      (is (= [nil nil] result)
          "make with nil should return [nil nil]")))

  (testing "make with custom stacktrace"
    (let [ex (Exception. "Custom error")
          custom-trace "Custom stacktrace here"
          [err stacktrace] (error-entry/make ex custom-trace)]
      (is (= ex err))
      (is (= custom-trace stacktrace)
          "make uses provided custom stacktrace instead of generating one"))))

(deftest error?-test
  (testing "error? returns true for actual errors"
    (let [ex (Exception. "test")
          error-tuple [ex "stacktrace"]]
      (is (true? (error/? error-tuple))
          "error? returns true when error tuple contains an exception")))

  (testing "error? returns false for no error"
    (is (false? (error/? [nil nil]))
        "error? returns false for [nil nil]"))

  (testing "error? returns false for partial error (nil exception)"
    (is (false? (error/? [nil "some stacktrace"]))
        "error? returns false when exception is nil even if stacktrace exists"))

  (testing "error? works with buckets"
    (let [ex (Exception. "bucket wrapped")
          error-bucket (bucket/grab :value :something :error [ex "stacktrace"])
          ok-bucket (bucket/grab :value :ok)]
      (is (true? (error/? error-bucket)))
      (is (false? (error/? ok-bucket))))))

(deftest format-error-test
  (testing "format-error with exception and stacktrace"
    (let [ex (Exception. "Format test error")
          stacktrace "java.lang.Exception: Format test error\n\tat test.clj:1"
          error-tuple [ex stacktrace]
          formatted (format/error-text error-tuple)]
      (is (str/includes? formatted "Error: Format test error"))
      (is (str/includes? formatted stacktrace)
          "formatted error includes message and stacktrace")))

  (testing "format-error with exception but no stacktrace"
    (let [ex (Exception. "No trace error")
          error-tuple [ex nil]
          formatted (format/error-text error-tuple)]
      (is (str/includes? formatted "Error: No trace error"))
      (is (not (str/includes? formatted "\n"))
          "formatted error includes message without newlines when stacktrace is nil")))

  (testing "format-error with no error returns nil"
    (let [no-error [nil nil]
          formatted (format/error-text no-error)]
      (is (nil? formatted)
          "format-error returns nil when there is no error"))))

(deftest handle-stdout-test
  (testing "handle with no error does nothing"
    (let [no-error [nil nil]
          output (with-out-str
                   (error/handle no-error :out :stdout :exit nil))]
      (is (empty? output)
          "handle produces no output when error is [nil nil]")))

  (testing "handle prints error details to stdout"
    (let [ex (RuntimeException. "Test logging error")
          error-tuple [ex "test stacktrace"]
          output (with-out-str
                   (error/handle error-tuple :out :stdout :exit nil))]
      (is (str/includes? output "error class: class java.lang.RuntimeException"))
      (is (str/includes? output "error message: Test logging error"))
      (is (str/includes? output "stacktrace:"))
      (is (str/includes? output "test stacktrace")
          "handle prints class, message, and stacktrace to stdout")))

  (testing "handle with :out :none produces no output"
    (let [ex (Exception. "Silent error")
          error-tuple [ex "silent stacktrace"]
          output (with-out-str
                   (error/handle error-tuple :out :none :exit nil))]
      (is (empty? output)
          "handle with :out :none produces no output")))

  (testing "handle prints exception cause when present"
    (let [cause (IllegalArgumentException. "Root cause")
          ex (RuntimeException. "Wrapper error" cause)
          error-tuple [ex nil]
          output (with-out-str
                   (error/handle error-tuple :out :stdout :exit nil))]
      (is (str/includes? output "error cause:"))
      (is (str/includes? output "IllegalArgumentException"))
      (is (str/includes? output "Root cause")
          "handle prints exception cause details when present")))

  (testing "handle handles error without stacktrace"
    (let [ex (Exception. "No stacktrace error")
          error-tuple [ex nil]
          output (with-out-str
                   (error/handle error-tuple :out :stdout :exit nil))]
      (is (str/includes? output "error message: No stacktrace error"))
      (is (not (str/includes? output "stacktrace:"))
          "handle prints message without stacktrace label when stacktrace is nil")))

  (testing "handle operates on buckets"
    (let [ex (RuntimeException. "Bucket error")
          bucket {:error [ex "bucket stacktrace"]}
          output (with-out-str
                   (error/handle bucket :out :stdout :exit nil))]
      (is (str/includes? output "Bucket error"))
      (is (str/includes? output "bucket stacktrace")))))

(deftest error-edge-cases-test
  (testing "error functions handle empty and nil inputs gracefully"
    (is (= [nil nil] (error-entry/make nil)))
    (is (= [nil nil] (error-entry/make nil nil)))
    (is (false? (error/? [])))
    (is (false? (error/? [nil])))
    (is (nil? (format/error-text [])))
    (is (nil? (format/error-text [nil]))
        "error functions handle nil and empty inputs without exceptions"))

  (testing "handle works with exceptions that have nil messages"
    (let [ex (RuntimeException.)
          error-tuple [ex nil]
          output (with-out-str
                   (error/handle error-tuple :out :stdout :exit nil))]
      (is (str/includes? output "error class:"))
      (is (str/includes? output "error message:")
          "handle handles nil exception messages gracefully")))

  (testing "error functions handle very long stacktraces"
    (let [deep-ex (Exception. "Deep error")
          long-stacktrace (str/join "\n" (repeat 1000 "at deep.class.method(file.java:123)"))
          error-tuple [deep-ex long-stacktrace]
          formatted (format/error-text error-tuple)]
      (is (str/includes? formatted "Error: Deep error"))
      (is (str/includes? formatted long-stacktrace)
          "error functions handle very long stacktraces without truncation"))))

(deftest handle-file-output-test
  (testing "handle writes to file with :out :file"
    (let [ex (RuntimeException. "File output test")
          error-tuple [ex "test stacktrace"]
          test-dir (str th/test-temp-root "/errors")]
      (error/handle error-tuple :out :file :dir test-dir :timestamp? false :name "test" :exit nil)
      (let [file (io/file (str test-dir "/test.err"))
            content (slurp file)]
        (is (.exists file))
        (is (str/includes? content "error class: class java.lang.RuntimeException"))
        (is (str/includes? content "error message: File output test"))
        (is (str/includes? content "test stacktrace")
            "handle writes error details to file"))))

  (testing "handle writes to file with timestamp"
    (let [ex (Exception. "Timestamp test")
          error-tuple [ex nil]
          test-dir (str th/test-temp-root "/errors")]
      (error/handle error-tuple :out :file :dir test-dir :timestamp? true :exit nil)
      (let [files (.listFiles (io/file test-dir))]
        (is (pos? (count files)))
        (is (str/ends-with? (.getName (first files)) ".err")
            "handle creates timestamped log files"))))

  (testing "handle with :out :both writes to file and stdout"
    (let [ex (Exception. "Both output test")
          error-tuple [ex "both stacktrace"]
          test-dir (str th/test-temp-root "/errors")
          stdout-output (with-out-str
                          (error/handle error-tuple :out :both :dir test-dir :timestamp? false :name "both-test" :exit nil))]
      (is (str/includes? stdout-output "error class:"))
      (is (str/includes? stdout-output "Both output test"))
      (let [file (io/file (str test-dir "/both-test.err"))
            content (slurp file)]
        (is (.exists file))
        (is (str/includes? content "Both output test")
            "handle with :out :both writes to both stdout and file"))))

  (testing "handle with :out :none does not write anything"
    (let [ex (Exception. "None output test")
          error-tuple [ex nil]
          test-dir (str th/test-temp-root "/errors-none")
          dir (io/file test-dir)
          before-count (if (.exists dir) (count (.listFiles dir)) 0)]
      (error/handle error-tuple :out :none :dir test-dir :exit nil)
      (let [after-count (if (.exists dir) (count (.listFiles dir)) 0)]
        (is (= before-count after-count)
            "handle with :out :none creates no files")))))
