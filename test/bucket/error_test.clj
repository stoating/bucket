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

(deftest wrap-error-test
  (testing "wrap-error catches exceptions and returns error tuple"
    (let [failing-fn (fn [_] (throw (RuntimeException. "Wrapped error")))
          wrapped-fn (error/wrap-error failing-fn)
          result (wrapped-fn "test-arg")]
      (is (vector? result))
      (is (= 2 (count result)))
      (let [[err stacktrace] result]
        (is (instance? RuntimeException err))
        (is (= "Wrapped error" (.getMessage err)))
        (is (string? stacktrace)
            "wrap-error catches exception and returns [exception stacktrace] tuple"))))

  (testing "wrap-error passes through successful results"
    (let [success-fn (fn [x y] (+ x y))
          wrapped-fn (error/wrap-error success-fn)
          result (wrapped-fn 5 10)]
      (is (= 15 result)
          "wrap-error passes through successful results unchanged")))

  (testing "wrap-error handles functions with no arguments"
    (let [no-arg-failing-fn (fn [] (throw (Exception. "No args error")))
          wrapped-fn (error/wrap-error no-arg-failing-fn)
          result (wrapped-fn)]
      (is (vector? result))
      (let [[err _] result]
        (is (instance? Exception err))
        (is (= "No args error" (.getMessage err))
            "wrap-error handles zero-arg functions that throw exceptions"))))

  (testing "wrap-error handles functions with multiple arguments"
    (let [multi-arg-fn (fn [a b c d] (if (= a "fail")
                                       (throw (Exception. "Multi arg error"))
                                       (+ a b c d)))
          wrapped-fn (error/wrap-error multi-arg-fn)]
      (is (= 10 (wrapped-fn 1 2 3 4)))
      (let [result (wrapped-fn "fail" 2 3 4)
            [err _] result]
        (is (instance? Exception err))
        (is (= "Multi arg error" (.getMessage err))
            "wrap-error handles multi-arg functions with both success and error cases")))))

(deftest with-context-test
  (testing "with-context adds context to existing error"
    (let [original-ex (RuntimeException. "Original error")
          original-error [original-ex "original stacktrace"]
          context "Processing failed during validation"
          [new-err stacktrace] (error/with-context original-error context)]
      (is (instance? clojure.lang.ExceptionInfo new-err))
      (is (= context (.getMessage new-err)))
      (is (= original-ex (:wrapped (ex-data new-err))))
      (is (= "original stacktrace" stacktrace)
          "with-context wraps error in ExceptionInfo with context message and preserves stacktrace")))

  (testing "with-context preserves no-error state"
    (let [no-error [nil nil]
          context "This won't be added"
          result (error/with-context no-error context)]
      (is (= no-error result)
          "with-context does not modify [nil nil] error tuples")))

  (testing "with-context handles error with nil stacktrace"
    (let [ex (Exception. "Base error")
          error-tuple [ex nil]
          context "Added context"
          [new-err stacktrace] (error/with-context error-tuple context)]
      (is (instance? clojure.lang.ExceptionInfo new-err))
      (is (= context (.getMessage new-err)))
      (is (= ex (:wrapped (ex-data new-err))))
      (is (nil? stacktrace)
          "with-context wraps error and preserves nil stacktrace")))

  (testing "with-context updates bucket error"
    (let [ex (Exception. "Bucket base error")
          bucket (bucket/grab :value :unchanged :error [ex "stacktrace"])
          context "Bucket context"
          updated (error/with-context bucket context)
          [wrapped stacktrace] (:error updated)]
      (is (= :unchanged (:value updated)))
      (is (instance? clojure.lang.ExceptionInfo wrapped))
      (is (= context (.getMessage wrapped)))
      (is (= ex (:wrapped (ex-data wrapped))))
      (is (= "stacktrace" stacktrace))
      (is (map? updated)))))

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

(deftest error-integration-test
  (testing "full error workflow: wrap -> make -> format -> handle"
    (let [risky-fn (fn [x] (if (= x "boom")
                             (throw (Exception. "Integration test error"))
                             (* x 2)))
          wrapped-fn (error/wrap-error risky-fn)
          error-result (wrapped-fn "boom")]
      (is (error/? error-result))
      (let [formatted (format/error-text error-result)]
        (is (str/includes? formatted "Error: Integration test error")))
      (let [output (with-out-str
                     (error/handle error-result :out :stdout :exit nil))]
        (is (str/includes? output "error message: Integration test error")
            "full workflow catches, formats, and handles errors correctly"))))

  (testing "error context chain"
    (let [base-error (error-entry/make (Exception. "Base problem"))
          step1-error (error/with-context base-error "Step 1 failed")
          step2-error (error/with-context step1-error "Step 2 failed")]
      (is (error/? step2-error))
      (let [[final-err _] step2-error]
        (is (= "Step 2 failed" (.getMessage final-err)))
        (let [step1-err (:wrapped (ex-data final-err))]
          (is (= "Step 1 failed" (.getMessage step1-err)))
          (let [base-err (:wrapped (ex-data step1-err))]
            (is (= "Base problem" (.getMessage base-err))
                "error context wraps previous contexts, creating a chain from outermost to base"))))))

  (testing "no error propagation through all functions"
    (let [no-error [nil nil]]
      (is (not (error/? no-error)))
      (is (nil? (format/error-text no-error)))
      (is (= no-error (error/with-context no-error "Won't be added")))
      (let [output (with-out-str
                     (error/handle no-error :out :stdout :exit nil))]
        (is (empty? output)
            "no-error [nil nil] passes through all functions unchanged")))))

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

  (testing "wrap-error handles nested exceptions correctly"
    (let [nested-fn (fn [] (throw (Exception. "Inner exception")))
          wrapper-fn (fn [] (try (nested-fn) (catch Exception e (throw (RuntimeException. "Outer exception" e)))))
          wrapped-fn (error/wrap-error wrapper-fn)
          [err _] (wrapped-fn)]
      (is (instance? RuntimeException err))
      (is (= "Outer exception" (.getMessage err)))
      (is (instance? Exception (.getCause err)))
      (is (= "Inner exception" (.getMessage (.getCause err)))
          "wrap-error preserves nested exception cause chain")))

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
      (let [file (io/file (str test-dir "/test.log"))
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
        (is (str/ends-with? (.getName (first files)) ".log")
            "handle creates timestamped log files"))))

  (testing "handle with :out :both writes to file and stdout"
    (let [ex (Exception. "Both output test")
          error-tuple [ex "both stacktrace"]
          test-dir (str th/test-temp-root "/errors")
          stdout-output (with-out-str
                          (error/handle error-tuple :out :both :dir test-dir :timestamp? false :name "both-test" :exit nil))]
      (is (str/includes? stdout-output "error class:"))
      (is (str/includes? stdout-output "Both output test"))
      (let [file (io/file (str test-dir "/both-test.log"))
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
