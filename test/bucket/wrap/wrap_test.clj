(ns bucket.wrap.wrap-test
  (:require [bucket :as bucket]
            [bucket.log :as log]
            [bucket.wrap :as wrap]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import [java.time Instant]))

(def work-ok
  (with-meta
    (fn [{:keys [logs]}]
      (bucket/grab :ok :logs (or logs [])))
    {:name 'work}))

(def work-boom
  (with-meta
    (fn [_]
      (throw (ex-info "explode" {:code 500})))
    {:name 'work}))

;;; Helper functions (moved near top for clarity)

(defn wrap-helper-level-3 [x]
  (println "Level 3: Processing" x)
  (* x 3))

(defn wrap-helper-level-2 [x]
  (println "Level 2: Starting with" x)
  (let [value (wrap-helper-level-3 x)]
    (println "Level 2: Got value" value)
    (+ value 10)))

(defn plain-nested [x]
  (println "Level 1: Begin nested processing")
  (let [intermediate (wrap-helper-level-2 x)]
    (println "Level 1: Intermediate value is" intermediate)
    (let [final (* intermediate 2)]
      (println "Level 1: Final value is" final)
      final)))

(def wrap-plain-nested
  (with-meta (bucket/bucketize plain-nested)
    {:name 'wrap-plain-nested}))

(defn add-three [x]
  (wrap-helper-level-2 x))

(defn add-four [x]
  (wrap-helper-level-2 x))

(defn log-thread [value message]
  (println message value)
  value)

(defn plain-threaded-nested [x]
  (-> x
      (log-thread "Thread Level 1: start")
      (add-three)
      (add-four)
      (log-thread "Thread Level 1: after helper")
      (* 3)))

(def wrap-threaded-nested
  (with-meta (bucket/bucketize plain-threaded-nested)
    {:name 'wrap-threaded-nested}))

(deftest wrap-success-path
  (testing "logging and success combined"
    (let [l0 {:indent 0 :time (Instant/parse "2024-01-15T10:30:00Z") :level :info :value "start"}
          f (wrap/wrap work-ok)
          resp (f {:logs [l0]})
          [_ enter args exit] (:logs resp)]
      (is (= {:id (:id resp)
              :name (str (:id resp) "-bucket")
              :meta {}
              :value :ok
              :error [nil nil]
              :logs [l0 enter args exit]}
             resp))
      (is (= "--> work" (:value enter)))
      (is (= "args: {}" (:value args)))
      (is (= (:indent enter) (:indent args)))
      (is (= "<-- work" (:value exit)))
      (is (= (:indent enter) (:indent exit)))
      (is (= (:indent l0) (:indent-next exit))
          "wrap adds entry/exit logs to successful execution"))))

(deftest wrap-exception-path
  (testing "exception caught with entry/exit logs"
    (let [l0 {:indent 2 :time (Instant/parse "2024-01-15T10:30:00Z") :level :debug :value "before"}
          f (wrap/wrap work-boom)
          resp (f {:logs [l0]})
          [_ lenter larg lexit] (:logs resp)]
      (is (nil? (:value resp)))
      (is (instance? Exception (first (:error resp))))
      (is (= [l0 lenter larg lexit] (:logs resp)))
      (is (= 6 (:indent lenter)))
      (is (= "args: {}" (:value larg)))
      (is (= 6 (:indent larg)))
      (is (= 6 (:indent lexit)))
      (is (= 2 (:indent-next lexit))
          "wrap catches exceptions and preserves entry/exit logging"))))

(deftest wrap-no-initial-logs
  (testing "logs start at zero with no initial logs"
    (let [f (wrap/wrap work-ok)
          resp (f {})
          [lenter larg lexit] (:logs resp)]
      (is (= :ok (:value resp)))
      (is (= 4 (:indent lenter)))
      (is (= "args: {}" (:value larg)))
      (is (= 4 (:indent larg)))
      (is (= 4 (:indent lexit)))
      (is (= 0 (:indent-next lexit))
          "wrap starts at indent 0 when no logs provided"))))

(deftest wrap-disable-args-logging
  (testing "log args can be disabled"
    (let [f (wrap/wrap work-ok {:log-args? false})
          resp (f {:foo "bar"})
          [entry exit] (:logs resp)]
      (is (= "--> work" (:value entry)))
      (is (= "<-- work" (:value exit)))
      (is (= 2 (count (:logs resp)))))))

(deftest wrap+-bucketizes-and-wraps
  (testing "wrap+ can bucketize plain functions on demand"
    (let [wrapped (wrap/wrap+ inc {:bucketize true})
          resp (wrapped (bucket/grab 5))]
      (is (= 6 (:value resp)))
      (is (= [nil nil] (:error resp))))))

(deftest wrap+-grab-option
  (testing "wrap+ grabs result when bucket opts provided"
    (let [resp (wrap/wrap+ inc 4 {:bucketize true
                                  :bucket {:meta {:job "plumber"}
                                           :name "my-bucket"}})]
      (is (= 5 (:value resp)))
      (is (= {:job "plumber"} (:meta resp)))
      (is (= "my-bucket" (:name resp)))
      (is (= [nil nil] (:error resp))))))

(deftest wrap+-grab-option-2
  (testing "wrap+ grabs result when bucket opts provided"
    (let [resp (wrap/wrap+ inc 4 {:bucketize true})]
      (is (= 5 (:value resp)))
      (is (= [nil nil] (:error resp))))))

(deftest wrap+-grab-option-3
  (testing "wrap+ grabs result when bucket opts provided"
    (let [resp (wrap/wrap+ inc {:bucketize true
                                :bucket {:meta {:job "plumber"}
                                         :name "my-bucket"}})]
      (is (fn? (:value resp)))
      (is (= {:job "plumber"} (:meta resp)))
      (is (= "my-bucket" (:name resp)))
      (is (= [nil nil] (:error resp)))
      (let [wrapped-inc (:value resp)
            result (wrapped-inc (bucket/grab 4))]
        (is (= 5 (:value result)))
        (is (= [nil nil] (:error result)))))))

(deftest wrap-nested-default-stack-indent
  (testing "nested stdout uses depth-driven indentation by default"
    (let [wrapped (wrap/wrap wrap-plain-nested {:redirect-mode :depth-aware})
          resp (wrapped (bucket/grab 5))
          logs (:logs resp)
          values (map :value logs)
          indents (map :indent logs)]
      (is (= 50 (:value resp)))
      (is (= [nil nil] (:error resp)))
      (is (= "--> wrap-plain-nested" (first values)))
      (is (str/starts-with? (second values) "args: "))
      (is (= ["Level 1: Begin nested processing"
              "Level 2: Starting with 5"
              "Level 3: Processing 5"
              "Level 2: Got value 15"
              "Level 1: Intermediate value is 25"
              "Level 1: Final value is 50"]
             (subvec (vec values) 2 8)))
      (is (= "<-- wrap-plain-nested" (last values)))
      (is (= [4 4 4 8 12 8 4 4 4] indents)))))

(deftest wrap-nested-stack-exclude-flattened
  (testing "stack exclusions flatten nested stdout indentation"
    (let [stack-opts {:stack-exclude {:exclude ["bucket.wrap.wrap_test$"] :mode :append}
                      :redirect-mode :depth-aware
                      :args-check-secrets false}
          wrapped (wrap/wrap wrap-plain-nested stack-opts)
          resp (wrapped (bucket/grab 5))
          simplify (fn [log] (select-keys log [:indent :value :indent-next]))
          actual (-> resp (assoc :logs (mapv simplify (:logs resp))))]
      (is (= {:id (:id resp)
              :name (:name resp)
              :meta (:meta resp)
              :value 50
              :error [nil nil]
              :logs [{:indent 4 :value "--> wrap-plain-nested" :indent-next 4}
                     {:indent 4 :value (:value (second (:logs resp))) :indent-next 4}
                     {:indent 4 :value "Level 1: Begin nested processing"}
                     {:indent 4 :value "Level 2: Starting with 5"}
                     {:indent 4 :value "Level 3: Processing 5"}
                     {:indent 4 :value "Level 2: Got value 15"}
                     {:indent 4 :value "Level 1: Intermediate value is 25"}
                     {:indent 4 :value "Level 1: Final value is 50"}
                     {:indent 4 :value "<-- wrap-plain-nested" :indent-next 0}]}
             actual)))))

(deftest wrap-threaded-indent
  (testing "threaded helper emits logs with consistent indentation"
    (let [wrapped (wrap/wrap wrap-threaded-nested {:args-check-secrets false
                                                   :redirect-mode :depth-aware})
          resp (wrapped (bucket/grab 5))
          values (map :value (:logs resp))
          indents (map :indent (:logs resp))]
      (is (= 255 (:value resp)))
      (is (= [nil nil] (:error resp)))
      (is (= "--> wrap-threaded-nested" (first values)))
      (is (str/starts-with? (second values) "args: "))
      (is (= ["Thread Level 1: start 5"
              "Level 2: Starting with 5"
              "Level 3: Processing 5"
              "Level 2: Got value 15"
              "Level 2: Starting with 25"
              "Level 3: Processing 25"
              "Level 2: Got value 75"
              "Thread Level 1: after helper 85"]
             (subvec (vec values) 2 10)))
      (is (= "<-- wrap-threaded-nested" (last values)))
      (is (= [4 4 4 8 12 8 8 12 8 4 4] indents)))))

(deftest wrap-nested-call-structure
  (testing "wrap captures nested calls with argument logging and captured output"
    (let [raw-inner (with-meta
                      (fn [{:keys [logs value]}]
                        (println "Level 3: processing" value)
                        (bucket/grab {:stage :inner
                                      :input value
                                      :value (* value 3)}
                                     :logs (or logs [])))
                      {:name 'inner-worker})
          wrapped-inner (wrap/wrap raw-inner)

          raw-middle (with-meta
                       (fn [{:keys [logs value]}]
                         (println "Level 2: start" value)
                         (let [inner (wrapped-inner {:logs (or logs [])
                                                     :value (inc value)})
                               inner-value (:value inner)]
                           (println "Level 2: combining" (:value inner-value))
                           (bucket/grab {:stage :middle
                                         :input value
                                         :inner inner-value}
                                        :logs (:logs inner))))
                       {:name 'middle-worker})
          wrapped-middle (wrap/wrap raw-middle)

          raw-outer (with-meta
                      (fn [{:keys [logs value]}]
                        (println "Level 1: begin" value)
                        (let [mid (wrapped-middle {:logs (or logs [])
                                                   :value (* 2 value)})
                              mid-value (:value mid)]
                          (println "Level 1: finish" (:stage mid-value))
                          (let [bucket (bucket/grab {:stage :outer
                                                     :input value
                                                     :middle mid-value}
                                                    :logs (:logs mid))]
                            bucket)))
                      {:name 'outer-worker})
          wrapped-outer (wrap/wrap raw-outer {:redirect-mode :depth-aware})
          response (-> (wrapped-outer {:value 5})
                       (update :logs log/log "post-outer-sentinel"))
          [outer-entry outer-args begin finish
           middle-entry middle-args
           level2-start level2-combine
           inner-entry inner-args
           level3 inner-exit middle-exit outer-exit sentinel] (:logs response)
          expected {:id (:id response)
                    :name (:name response)
                    :meta {}
                    :value {:stage :outer
                            :input 5
                            :middle {:stage :middle
                                     :input 10
                                     :inner {:stage :inner
                                             :input 11
                                             :value 33}}}
                    :error [nil nil]
                    :logs [{:indent 4 :time (:time outer-entry) :level :info :value "--> outer-worker" :indent-next 4}
                           {:indent 4 :time (:time outer-args) :level :info :value "args: {:value 5}" :indent-next 4}
                           {:indent 4 :time (:time begin) :level :info :value "Level 1: begin 5"}
                           {:indent 4 :time (:time finish) :level :info :value "Level 1: finish :middle"}
                           {:indent 8 :time (:time middle-entry) :level :info :value "--> middle-worker" :indent-next 8}
                           {:indent 8 :time (:time middle-args) :level :info :value "args: {:value 10}" :indent-next 8}
                           {:indent 8 :time (:time level2-start) :level :info :value "Level 2: start 10"}
                           {:indent 8 :time (:time level2-combine) :level :info :value "Level 2: combining 33"}
                           {:indent 12 :time (:time inner-entry) :level :info :value "--> inner-worker" :indent-next 12}
                           {:indent 12 :time (:time inner-args) :level :info :value "args: {:value 11}" :indent-next 12}
                           {:indent 12 :time (:time level3) :level :info :value "Level 3: processing 11"}
                           {:indent 12 :time (:time inner-exit) :level :info :value "<-- inner-worker" :indent-next 8}
                           {:indent 8 :time (:time middle-exit) :level :info :value "<-- middle-worker" :indent-next 4}
                           {:indent 4 :time (:time outer-exit) :level :info :value "<-- outer-worker" :indent-next 0}
                           {:indent 0 :time (:time sentinel) :level :info :value "post-outer-sentinel"}]}]
      (is (= expected response)))))
