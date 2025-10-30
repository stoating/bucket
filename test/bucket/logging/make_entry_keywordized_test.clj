(ns bucket.logging.make-entry-keywordized-test
  "Tests for logging make-entry function with keyword arguments."
  (:require [bucket.log.entry :as log-entry]
            [clojure.test :refer [deftest is testing]]))

(deftest make-entry-keyword-value-only-test
  (testing "make-entry with only :value keyword"
    (let [entry (log-entry/make :value "Test message")]
      (is (= "Test message" (:value entry)))
      (is (= :info (:level entry)))
      (is (= 0 (:indent entry)))
      (is (number? (:time entry)))
      (is (pos? (:time entry))
          "Entry contains message, defaults to :info level and 0 indent, has positive timestamp"))))

(deftest make-entry-keyword-value-and-level-test
  (testing "make-entry with :value and :level keywords"
    (let [entry (log-entry/make :value "Warning message" :level :warning)]
      (is (= "Warning message" (:value entry)))
      (is (= :warning (:level entry)))
      (is (= 0 (:indent entry)))
      (is (number? (:time entry))
          "Entry contains message, has specified :warning level, defaults to 0 indent, has timestamp"))))

(deftest make-entry-keyword-value-and-indent-test
  (testing "make-entry with :value and :indent keywords"
    (let [entry (log-entry/make :value "Indented message" :indent 3)]
      (is (= "Indented message" (:value entry)))
      (is (= :info (:level entry)))
      (is (= 3 (:indent entry)))
      (is (number? (:time entry))
          "Entry contains message, defaults to :info level, has specified indent level, has timestamp"))))

(deftest make-entry-keyword-all-three-test
  (testing "make-entry with :value, :level, and :indent keywords"
    (let [entry (log-entry/make :value "Full message" :level :error :indent 2)]
      (is (= "Full message" (:value entry)))
      (is (= :error (:level entry)))
      (is (= 2 (:indent entry)))
      (is (number? (:time entry))
          "Entry contains message with specified :error level and indent level, has timestamp"))))

(deftest make-entry-keyword-different-order-test
  (testing "make-entry with keywords in different order"
    (let [entry1 (log-entry/make :level :critical :value "Order test" :indent 1)
          entry2 (log-entry/make :indent 1 :level :critical :value "Order test")]
      (is (= "Order test" (:value entry1)))
      (is (= :critical (:level entry1)))
      (is (= 1 (:indent entry1)))
      (is (= "Order test" (:value entry2)))
      (is (= :critical (:level entry2)))
      (is (= 1 (:indent entry2)))
      (is (= (:value entry1) (:value entry2)))
      (is (= (:level entry1) (:level entry2)))
      (is (= (:indent entry1) (:indent entry2))
          "Keyword order doesn't affect the resulting entry values"))))

(deftest make-entry-positional-value-with-level-keyword-test
  (testing "make-entry with positional value followed by :level keyword"
    (let [entry (log-entry/make "My message" :level :warning)]
      (is (= "My message" (:value entry)))
      (is (= :warning (:level entry)))
      (is (= 0 (:indent entry)))
      (is (number? (:time entry))
          "Entry contains message as first positional arg, has specified :warning level, defaults to 0 indent"))))

(deftest make-entry-positional-value-with-indent-keyword-test
  (testing "make-entry with positional value followed by :indent keyword"
    (let [entry (log-entry/make "Simple message" :indent 3)]
      (is (= "Simple message" (:value entry)))
      (is (= :info (:level entry)))
      (is (= 3 (:indent entry)))
      (is (number? (:time entry))
          "Entry contains message as positional arg, defaults to :info level, has specified indent via keyword"))))

(deftest make-entry-positional-value-with-both-keywords-test
  (testing "make-entry with positional value followed by :level and :indent keywords"
    (let [entry (log-entry/make "Full message" :level :error :indent 2)]
      (is (= "Full message" (:value entry)))
      (is (= :error (:level entry)))
      (is (= 2 (:indent entry)))
      (is (number? (:time entry))
          "Entry contains message as positional arg with both level and indent specified via keywords"))))


