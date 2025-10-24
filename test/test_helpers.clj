(ns test-helpers
  (:require [bucket.spouts.extract :as spouts]
            [clojure.java.io :as io]
            [clojure.test :refer [is]]))

(defn assert-no-error
  "Assert that a bucket response has no error. If there is an error,
   print error."
  [bucket]
  (let [err (first (spouts/drain-error bucket))]
    (is (nil? err))))

(defn assert-error
  "Assert that a bucket response has an error."
  ([bucket]
   (let [err (first (spouts/drain-error bucket))]
     (is (some? err) "Expected an error but got none"))))

(def test-temp-root "test_temp")

(defn- delete-recursively! [^java.io.File f]
  (when (.exists f)
    (when (.isDirectory f)
      (doseq [child (.listFiles f)]
        (delete-recursively! child)))
    (.delete f)))

(defn clean-test-temp! []
  (let [dir (io/file test-temp-root)]
    (when (.exists dir)
      (delete-recursively! dir))
    (.mkdirs dir)
    dir))

(defn clean-test-temp-fixture [f]
  (clean-test-temp!)
  (try
    (f)
    (finally (clean-test-temp!))))
