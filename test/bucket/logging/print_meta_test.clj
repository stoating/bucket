(ns bucket.logging.print-meta-test
  "Tests for logging print function."
  (:require [bucket.meta :as meta]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [test-helpers :as th]))

(use-fixtures :each th/clean-test-temp-fixture)

(deftest ->stdout-test
  (testing "metadata with :stdout output"
    (let [meta-data {:user "alice" :timestamp 1234567890}
          out-str (with-out-str
                    (meta/print meta-data :out :stdout :name "test-bucket"))]
      (is (.contains out-str "=== Bucket Metadata ==="))
      (is (.contains out-str ":user"))
      (is (.contains out-str "alice"))
      (is (.contains out-str ":timestamp"))
      (is (.contains out-str "1234567890")
          "print writes formatted metadata to stdout"))))

(deftest ->file-test
  (testing "metadata with :file output"
    (let [meta-data {:operation "test" :version "1.0"}
          bucket-name "test-bucket"
          temp-dir th/test-temp-root
          temp-dir-file (io/file temp-dir)
          out-str (with-out-str
                    (meta/print meta-data :out :file :name bucket-name :dir temp-dir))]
      (is (.contains out-str "Metadata written to:"))
      (is (.contains out-str bucket-name))
      (is (.contains out-str ".edn"))
      (is (.contains out-str "test-bucket.edn"))
      (is (= 1 (count (.listFiles temp-dir-file)))
          "print creates a single .edn file in the specified directory"))))

(deftest print-to-both-test
  (testing "metadata with :both output"
    (let [meta-data {:env "prod" :tags ["test"]}
          bucket-name "both-bucket"
          temp-dir th/test-temp-root
          temp-dir-file (io/file temp-dir)
          out-str (with-out-str
                    (meta/print meta-data :out :both :name bucket-name :dir temp-dir))]
      (is (.contains out-str "=== Bucket Metadata ==="))
      (is (.contains out-str ":env"))
      (is (.contains out-str "prod"))
      (is (.contains out-str "Metadata written to:"))
      (is (.contains out-str "both-bucket.edn"))
      (is (= 1 (count (.listFiles temp-dir-file)))
          "print outputs to stdout and creates file when :both is specified"))))

(deftest print-to-none-test
  (testing "metadata with :none output"
    (let [meta-data {:should-not-appear "anywhere"}
          bucket-name "none-bucket"
          temp-dir th/test-temp-root
          temp-dir-file (io/file temp-dir)
          out-str (with-out-str
                    (meta/print meta-data :out :none :name bucket-name :dir temp-dir))]
      (is (= "" out-str))
      (is (zero? (count (.listFiles temp-dir-file)))
          "print produces no output when :none is specified"))))

(deftest print-complex-structure-test
  (testing "nested metadata structures"
    (let [meta-data {:user {:name "bob" :id 42}
                     :context {:environment "staging" :region "us-west"}
                     :tags ["important" "monitored"]
                     :config {:retries 3 :timeout 5000}}
          out-str (with-out-str
                    (meta/print meta-data :out :stdout :name "complex-bucket"))]
      (is (.contains out-str "=== Bucket Metadata ==="))
      (is (.contains out-str ":user"))
      (is (.contains out-str ":name"))
      (is (.contains out-str "bob"))
      (is (.contains out-str ":context"))
      (is (.contains out-str ":environment"))
      (is (.contains out-str "staging"))
      (is (.contains out-str ":tags"))
      (is (.contains out-str "important")
          "print handles nested maps and collections in metadata"))))

(deftest print-with-custom-name-test
  (testing "custom bucket name in file output"
    (let [meta-data {:custom "name"}
          custom-name "mycustom"
          temp-dir th/test-temp-root
          temp-dir-file (io/file temp-dir)
          out-str (with-out-str
                    (meta/print meta-data :out :file :name custom-name :dir temp-dir))]
      (is (.contains out-str "Metadata written to:"))
      (is (.contains out-str (str custom-name ".edn")))
      (is (= 1 (count (.listFiles temp-dir-file))))
      (let [created-file (first (.listFiles temp-dir-file))]
        (is (.contains (.getName created-file) custom-name)
            "print uses the :name parameter in the output filename")))))
