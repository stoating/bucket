(ns monad-test
  (:require [bucket :as bucket]
            [clojure.test :refer [deftest is testing]]
            [monad :as monad])
  (:import [java.time Instant]))

(deftest pure-and-bind
  (testing "pure wraps values and optional logs"
    (let [log {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "note"}
          monad-one (monad/pure 42)
          monad-two (monad/pure "hi" :logs [log])]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result 42
              :error [nil nil]
              :logs []}
             monad-one))
      (is (= {:id (:id monad-two)
              :name (str (:id monad-two) "-bucket")
              :meta {}
              :result "hi"
              :error [nil nil]
              :logs [log]}
             monad-two)
          "pure creates bucket with result and optional logs")))

  (testing "bind chains results and accumulates logs"
    (let [l1 {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "first"}
          l2 {:indent 1 :time (Instant/parse "2024-01-15T10:30:01Z") :level :debug :value "second"}
          monad-one (monad/pure 5 :logs [l1])
          res (-> monad-one
                  (monad/bind (fn [x] (monad/pure (* x 2) :logs [l2]))))]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result 5
              :error [nil nil]
              :logs [l1]}
             monad-one))
      (is (= {:id (:id res)
              :name (str (:id res) "-bucket")
              :meta {}
              :result 10
              :error [nil nil]
              :logs [l1 l2]}
             res)
          "bind chains computations and accumulates logs from all steps")))

  (testing "bind short-circuits on error"
    (let [err (bucket/grab :error ["boom"])
          res (monad/bind err (fn [_] (monad/pure :never)))]
      (is (= err res)
          "bind returns error immediately without executing function"))))

(deftest fmap-and-lift
  (testing "fmap transforms values and preserves logs"
    (let [l {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "ok"}
          res (monad/fmap inc (monad/pure 5 :logs [l]))]
      (is (= {:id (:id res)
              :name (str (:id res) "-bucket")
              :meta {}
              :result 6
              :error [nil nil]
              :logs [l]}
             res)
          "fmap applies function to result while preserving logs")))

  (testing "lift composes like fmap"
    (let [inc* (monad/lift inc)
          str* (monad/lift str)
          res (-> (monad/pure 9) inc* str*)
          alt (monad/fmap (comp str inc) (monad/pure 9))]
      (is (= {:id (:id res)
              :name (str (:id res) "-bucket")
              :meta {}
              :result "10"
              :error [nil nil]
              :logs []}
             res))
      (is (= (select-keys res [:result :error :logs :meta])
             (select-keys alt [:result :error :logs :meta]))
          "lift creates composable monadic functions equivalent to fmap")))

  (testing "fmap/lift short-circuit on error"
    (let [err (bucket/grab :error ["bad"])
          res1 (monad/fmap inc err)
          res2 ((monad/lift inc) err)]
      (is (= err res1))
      (is (= err res2)
          "fmap and lift propagate errors without applying function"))))

(deftest join-sequence-and-mapm
  (testing "join flattens and merges logs"
    (let [l1 {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "outer"}
          l2 {:indent 1 :time (Instant/parse "2024-01-15T10:30:01Z") :level :debug :value "inner"}
          monad-one (monad/pure 7 :logs [l2])
          monad-two (monad/pure monad-one :logs [l1])
          res (monad/join monad-two)]
      (is (= {:id (:id monad-one)
              :name (str (:id monad-one) "-bucket")
              :meta {}
              :result 7
              :error [nil nil]
              :logs [l2]}
             monad-one))
      (is (= {:id (:id monad-two)
              :name (str (:id monad-two) "-bucket")
              :meta {}
              :result monad-one
              :error [nil nil]
              :logs [l1]}
             monad-two))
      (is (= {:id (:id res)
              :name (str (:id res) "-bucket")
              :meta {}
              :result 7
              :error [nil nil]
              :logs [l1 l2]}
             res)
          "join flattens nested bucket and merges all logs")))

  (testing "sequence-m collects results; fails fast"
    (let [ok (monad/pure 1)
          err (bucket/grab :error ["oops"])
          r1 (monad/sequence-m [(monad/pure 1) (monad/pure 2) (monad/pure 3)])
          r2 (monad/sequence-m [ok err ok])]
      (is (= {:id (:id r1)
              :name (str (:id r1) "-bucket")
              :meta {}
              :result [1 2 3]
              :error [nil nil]
              :logs []}
             r1))
      (is (= {:id (:id r2)
              :name (str (:id r2) "-bucket")
              :meta {}
              :result nil
              :error ["oops"]
              :logs []}
             r2)
          "sequence-m collects all results or fails fast on first error")))

  (testing "map-m applies monadic fn"
    (let [f (fn [x] (monad/pure (* x 2)))
          g (fn [_] (bucket/grab :error ["x"]))
          r-ok (monad/map-m f [1 2])
          r-err (monad/map-m g [1 2])]
      (is (= {:id (:id r-ok)
              :name (str (:id r-ok) "-bucket")
              :meta {}
              :result [2 4]
              :error [nil nil]
              :logs []}
             r-ok))
      (is (= {:id (:id r-err)
              :name (str (:id r-err) "-bucket")
              :meta {}
              :result nil
              :error ["x"]
              :logs []}
             r-err)
          "map-m applies monadic function to each element or fails fast"))))

(deftest kleisli-composition
  (testing ">> composes monadic fns"
    (let [f (fn [x] (monad/pure (+ x 1)))
          g (fn [x] (monad/pure (* x 3)))
          h (monad/>> f g)
          r (h 3)]
      (is (= {:id (:id r)
              :name (str (:id r) "-bucket")
              :meta {}
              :result 12
              :error [nil nil]
              :logs []}
             r)
          ">> composes monadic functions left-to-right")))

  (testing ">> short-circuits on error"
    (let [bad (fn [_] (bucket/grab :error ["bad"]))
          never (fn [_] (monad/pure :never))
          h (monad/>> bad never)
          r (h :x)]
      (is (= {:id (:id r)
              :name (str (:id r) "-bucket")
              :meta {}
              :result nil
              :error ["bad"]
              :logs []}
             r)
          ">> stops at first error without executing remaining functions"))))
