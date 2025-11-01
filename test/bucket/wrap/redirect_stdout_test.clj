(ns bucket.wrap.redirect-stdout-test
  (:require [bucket :as bucket]
            [bucket.log :as log]
            [bucket.wraps.wrapper.redirect-stdout :as wrap]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn bucket-view
  "Select the core fields we care about when asserting bucket structure."
  [bucket]
  (select-keys bucket [:id :name :meta :value :logs :error]))

;; Test functions that use various output methods

(defn ^{:name test-fn-println}
  test-fn-println
  [{:keys [logs]}]
  (println "Line 1 from println")
  (println "Line 2 from println")
  (bucket/grab :println-result :logs (or logs [])))

(defn ^{:name test-fn-pr}
  test-fn-pr
  [{:keys [logs]}]
  (pr {:type "pr output" :data 123})
  (pr [:vector "with" "items"])
  (bucket/grab :pr-result :logs (or logs [])))

(defn ^{:name test-fn-print}
  test-fn-print
  [{:keys [logs]}]
  (print "First part ")
  (print "second part ")
  (print "third part")
  (bucket/grab :print-result :logs (or logs [])))

(defn ^{:name test-fn-mixed}
  test-fn-mixed
  [{:keys [logs]}]
  (println "Starting with println")
  (pr {:status "processing"})
  (print "\nUsing print now")
  (println "\nBack to println")
  (bucket/grab :mixed-result :logs (or logs [])))

(defn ^{:name test-fn-printf}
  test-fn-printf
  [{:keys [logs]}]
  (printf "Formatted: %d items, %.2f%% complete\n" 42 98.5)
  (bucket/grab :printf-result :logs (or logs [])))

(defn ^{:name test-fn-no-output}
  test-fn-no-output
  [{:keys [logs]}]
  ;; Function that produces no output
  (bucket/grab :silent-result :logs (or logs [])))

(defn ^{:name test-fn-with-own-logs}
  test-fn-with-own-logs
  [{:keys [logs]}]
  (println "Captured output")
  (let [logs-with-entry (log/log (or logs []) "Function's own log" :level :warning)]
    (bucket/grab :mixed-logs-result :logs logs-with-entry)))

(defn stack-inner []
  (println "Level 2: inner operation"))

(defn stack-outer []
  (println "Level 1: outer operation")
  (stack-inner))

(defn ^{:name stack-fn}
  stack-fn
  [{:keys [logs]}]
  (stack-outer)
  (bucket/grab :stack-output :logs (or logs [])))

(defn capture-level-logs [logs]
  (->> logs
       (filter #(and (:value %) (str/starts-with? (:value %) "Level ")))
       vec))

(defn capture-level-diff [wrapped]
  (let [resp (wrapped {:logs []})
        level-logs (capture-level-logs (:logs resp))]
    {:resp resp
     :logs level-logs
     :diff (when (= 2 (count level-logs))
             (- (:indent (second level-logs))
                (:indent (first level-logs))))}))

(deftest stack-exclude-default-append
  (testing "default stack exclusion retains nested indentation"
    (let [{:keys [logs diff]} (capture-level-diff (wrap/redirect-stdout stack-fn :mode :depth-aware))]
      (is (= 2 (count logs)))
      (is (pos? diff)))))

(deftest stack-exclude-explicit-append
  (testing "appending custom exclusions flattens indentation"
    (let [{default-diff :diff} (capture-level-diff (wrap/redirect-stdout stack-fn :mode :depth-aware))
          {:keys [logs diff]} (capture-level-diff
                               (wrap/redirect-stdout stack-fn
                                                     :stack-exclude {:exclude ["fb.core.wrap."]}))]
      (is (= 2 (count logs)))
      (is (<= diff 0))
      (is (pos? default-diff))
      (is (< diff default-diff)))))

(deftest stack-exclude-overwrite
  (testing "overwriting exclusions increases observed depth"
    (let [{default-diff :diff} (capture-level-diff (wrap/redirect-stdout stack-fn))
          {:keys [logs diff]} (capture-level-diff
                               (wrap/redirect-stdout stack-fn
                                                     :stack-exclude {:exclude [] :mode :replace}))]
      (is (= 2 (count logs)))
      (is (>= diff default-diff)))))

(deftest redirect-stdout-println
  (testing "captures println output"
    (let [f (wrap/redirect-stdout test-fn-println)
          resp (f {:logs []})
          [log1 log2] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value :println-result
                    :error [nil nil]
                    :logs [(assoc log1 :value "Line 1 from println" :level :info)
                           (assoc log2 :value "Line 2 from println" :level :info)]}]
      (is (= expected (bucket-view resp))))))

(deftest redirect-stdout-pr
  (testing "captures pr output"
    (let [f (wrap/redirect-stdout test-fn-pr)
          resp (f {:logs []})
          [log] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value :pr-result
                    :error [nil nil]
                    :logs [(assoc log :level :info)]}]
      (is (= expected (bucket-view resp)))
      (is (re-find #"\{:type \"pr output\"" (:value log))))))

(deftest redirect-stdout-print
  (testing "captures print output"
    (let [f (wrap/redirect-stdout test-fn-print)
          resp (f {:logs []})
          [log] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value :print-result
                    :error [nil nil]
                    :logs [(assoc log :value "First part second part third part"
                                   :level :info)]}]
      (is (= expected resp)))))

(deftest redirect-stdout-mixed
  (testing "captures mixed output types"
    (let [f (wrap/redirect-stdout test-fn-mixed :mode :depth-aware)
          resp (f {:logs []})
          [log1 log2 log3 log4 & remainder] (:logs resp)
          adjusted-logs (vec (concat [(assoc log1 :value "Starting with println" :level :info :indent 0)
                                      (assoc log2 :level :info)
                                      (assoc log3 :level :info)
                                      (assoc log4 :level :info)]
                                     remainder))
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value :mixed-result
                    :error [nil nil]
                    :logs adjusted-logs}]
      (is (= expected resp)))))

(deftest redirect-stdout-printf
  (testing "captures printf output"
    (let [f (wrap/redirect-stdout test-fn-printf)
          resp (f {:logs []})
          [log] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value :printf-result
                    :error [nil nil]
                    :logs [(assoc log :value "Formatted: 42 items, 98.50% complete"
                                   :level :info)]}]
      (is (= expected (bucket-view resp))))))

(deftest redirect-stdout-no-output
  (testing "handles functions with no output"
    (let [f (wrap/redirect-stdout test-fn-no-output)
          resp (f {:logs []})
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value :silent-result
                    :error [nil nil]
                    :logs []}]
      (is (= expected (bucket-view resp))))))

(deftest redirect-stdout-preserves-existing-logs
  (testing "preserves logs passed in opts"
    (let [existing-log {:indent 0
                        :time (System/currentTimeMillis)
                        :level :debug
                        :value "Pre-existing log"}
          f (wrap/redirect-stdout test-fn-println)
          resp (f {:logs [existing-log]})
          [log1 log2 log3] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value :println-result
                    :error [nil nil]
                    :logs [log1
                           (assoc log2 :value "Line 1 from println" :level :info)
                           (assoc log3 :value "Line 2 from println" :level :info)]}]
      (is (= expected (bucket-view resp))))))

(deftest redirect-stdout-with-function-logs
  (testing "preserves logs that function creates"
    (let [f (wrap/redirect-stdout test-fn-with-own-logs)
          resp (f {:logs []})
          [log1 log2] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value :mixed-logs-result
                    :error [nil nil]
                    :logs [(assoc log1 :value "Captured output" :level :info)
                           (assoc log2 :value "Function's own log" :level :warning)]}]
      (is (= expected (bucket-view resp))))))

(deftest redirect-stdout-indent-inheritance
  (testing "captured logs inherit indent from existing logs"
    (let [indented-log {:indent 4
                        :time (System/currentTimeMillis)
                        :level :info
                        :value "Indented context"}
          f (wrap/redirect-stdout test-fn-println)
          resp (f {:logs [indented-log]})
          [log1 log2 log3] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value :println-result
                    :error [nil nil]
                    :logs [log1
                           (assoc log2 :value "Line 1 from println" :level :info :indent 4)
                           (assoc log3 :value "Line 2 from println" :level :info :indent 4)]}]
      (is (= expected (bucket-view resp))))))

(deftest redirect-stdout-empty-lines-filtered
  (testing "empty lines are filtered out"
    (let [f (wrap/redirect-stdout
              (fn [{:keys [logs]}]
                (println "Line 1")
                (println "")
                (println "")
                (println "Line 2")
                (bucket/grab :filtered-result :logs (or logs []))))
          resp (f {:logs []})
          [log1 log2] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value :filtered-result
                    :error [nil nil]
                    :logs [(assoc log1 :value "Line 1" :level :info)
                           (assoc log2 :value "Line 2" :level :info)]}]
      (is (= expected (bucket-view resp))))))

(deftest redirect-stdout-check-secrets-false
  (testing "captured output does not trigger password redaction"
    (let [f (wrap/redirect-stdout
              (fn [{:keys [logs]}]
                (println "password=secret123")
                (println "api_key=abc123")
                (bucket/grab :no-redaction :logs (or logs []))))
          resp (f {:logs []})
          [log1 log2] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value :no-redaction
                    :error [nil nil]
                    :logs [(assoc log1 :value "password=secret123" :level :info)
                           (assoc log2 :value "api_key=abc123" :level :info)]}]
      (is (= expected (bucket-view resp)))
      (is (not= "* log redacted *" (:value log1))))))

(deftest redirect-stdout-preserves-metadata
  (testing "preserves function metadata"
    (let [original-fn (fn [{:keys [logs]}]
                        (bucket/grab :value :logs (or logs [])))
          meta-fn (with-meta original-fn {:name "test-function" :doc "Test doc"})
          wrapped (wrap/redirect-stdout meta-fn)]
      (is (= {:name "test-function" :doc "Test doc"} (meta wrapped)))
      "Function metadata preserved through wrapping")))

(deftest redirect-stdout-bucket-structure
  (testing "returns proper bucket structure"
    (let [f (wrap/redirect-stdout test-fn-println)
          resp (f {:logs []})
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value :println-result
                    :logs (:logs resp)
                    :error [nil nil]}]
      (is (= expected (bucket-view resp))))))

;; Tests for redirect-stdout-lifted

(defn plain-fn-simple [x]
  (println "Simple processing:" x)
  (* x 2))

(defn plain-fn-with-pr [x]
  (pr {:input x})
  (+ x 10))

(defn plain-fn-mixed [x]
  (println "Start")
  (pr {:value x})
  (print "\nMiddle")
  (println "\nEnd")
  (str "Result: " x))

(deftest redirect-stdout-lifted-basic
  (testing "captures output from bucketized function"
    (let [lifted (bucket/bucketize plain-fn-simple)
          f (wrap/redirect-stdout lifted)
          resp (f (bucket/grab 5 :logs []))
          [log] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value 10
                    :error [nil nil]
                    :logs [(assoc log :value "Simple processing: 5" :level :info)]}]
      (is (= expected (bucket-view resp))))))

(deftest redirect-stdout-lifted-pr
  (testing "captures pr output from bucketized function"
    (let [lifted (bucket/bucketize plain-fn-with-pr)
          f (wrap/redirect-stdout lifted)
          resp (f (bucket/grab 3))
          [log] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value 13
                    :error [nil nil]
                    :logs [(assoc log :level :info)]}]
      (is (= expected (bucket-view resp)))
      (is (re-find #"\{:input 3\}" (:value log))))))

(deftest redirect-stdout-lifted-mixed
  (testing "captures mixed output from bucketized function"
    (let [lifted (bucket/bucketize plain-fn-mixed)
          f (wrap/redirect-stdout lifted)
          resp (f (bucket/grab "test"))
          [log1 log2 log3 log4] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value "Result: test"
                    :error [nil nil]
                    :logs [(assoc log1 :value "Start" :level :info :indent 0)
                           (assoc log2 :level :info)
                           (assoc log3 :level :info)
                           (assoc log4 :value "End" :level :info :indent 0)]}]
      (is (= expected (bucket-view resp))))))

(deftest redirect-stdout-lifted-preserves-logs
  (testing "preserves existing logs in bucket"
    (let [existing-log {:indent 0
                        :time (System/currentTimeMillis)
                        :level :debug
                        :value "Pre-existing"}
          lifted (bucket/bucketize plain-fn-simple)
          f (wrap/redirect-stdout lifted)
          resp (f (bucket/grab 5 :logs [existing-log]))
          [log1 log2] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value 10
                    :error [nil nil]
                    :logs [log1
                           (assoc log2 :value "Simple processing: 5" :level :info)]}]
      (is (= expected (bucket-view resp))))))

(deftest redirect-stdout-lifted-indent-inheritance
  (testing "captured logs inherit indent"
    (let [indented-log {:indent 2
                        :time (System/currentTimeMillis)
                        :level :info
                        :value "Indented"}
          lifted (bucket/bucketize plain-fn-simple)
          f (wrap/redirect-stdout lifted)
          resp (f (bucket/grab 5 :logs [indented-log]))
          [log1 log2] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value 10
                    :error [nil nil]
                    :logs [log1
                           (assoc log2 :value "Simple processing: 5" :level :info :indent 2)]}]
      (is (= expected (bucket-view resp))))))

(deftest redirect-stdout-lifted-no-output
  (testing "handles functions with no output"
    (let [silent-fn (fn [x] (* x 3))
          lifted (bucket/bucketize silent-fn)
          f (wrap/redirect-stdout lifted)
          resp (f (bucket/grab 4))
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value 12
                    :error [nil nil]
                    :logs []}]
      (is (= expected (bucket-view resp))))))

(deftest redirect-stdout-lifted-composition
  (testing "can compose with other bucket operations"
    (let [lifted (bucket/bucketize plain-fn-simple)
          captured (wrap/redirect-stdout lifted)
          resp (bucket/stir-in inc (captured (bucket/grab 5)))
          [log] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value 11
                    :error [nil nil]
                    :logs [(assoc log :value "Simple processing: 5" :level :info)]}]
      (is (= expected (bucket-view resp))))))

;; Tests for multi-argument functions via destructuring

(defn plain-fn-two-args [[x y]]
  (println "Computing sum of" x "and" y)
  (pr {:x x :y y})
  (println "\nDone computing")
  (+ x y))

(deftest redirect-stdout-lifted-two-args
  (testing "captures output from function with two arguments via destructuring"
    (let [lifted (bucket/bucketize plain-fn-two-args)
          f (wrap/redirect-stdout lifted)
          resp (f (bucket/grab [10 20]))
          [log1 log2 log3] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value 30
                    :error [nil nil]
                    :logs [(assoc log1 :value "Computing sum of 10 and 20" :level :info)
                           (assoc log2 :level :info)
                           (assoc log3 :value "Done computing" :level :info)]}]
      (is (= expected (bucket-view resp)))
      (is (re-find #"\{:x 10, :y 20\}" (:value log2))))))

(defn plain-fn-three-args [[x y z]]
  (println "Values:" x y z)
  (print "Sum: ")
  (println (+ x y z))
  (+ x y z))

(deftest redirect-stdout-lifted-three-args
  (testing "works with three arguments via vector destructuring"
    (let [lifted (bucket/bucketize plain-fn-three-args)
          f (wrap/redirect-stdout lifted)
          resp (f (bucket/grab [1 2 3]))
          [log1 log2] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value 6
                    :error [nil nil]
                    :logs [(assoc log1 :value "Values: 1 2 3" :level :info)
                           (assoc log2 :level :info)]}]
      (is (= expected (bucket-view resp))))))

(defn helper-fn-level-3 [x]
  (println "Level 3: Processing" x)
  (* x 3))

(defn helper-fn-level-2 [x]
  (println "Level 2: Starting with" x)
  (let [result (helper-fn-level-3 x)]
    (println "Level 2: Got result" result)
    (+ result 10)))

(defn plain-fn-nested [x]
  (println "Level 1: Begin nested processing")
  (let [intermediate (helper-fn-level-2 x)]
    (println "Level 1: Intermediate result is" intermediate)
    (let [final (* intermediate 2)]
      (println "Level 1: Final result is" final)
      final)))

(deftest redirect-stdout-lifted-nested-functions
  (testing "captures output from nested function calls"
    (let [lifted (bucket/bucketize plain-fn-nested)
          f (wrap/redirect-stdout lifted :mode :depth-aware)
          input-bucket (bucket/grab 5 :logs [])
          result-bucket (f input-bucket)]
      ;; Verify the complete bucket structure
      (is (= {:id (:id input-bucket)
              :name (str (:id input-bucket) "-bucket")
              :meta {}
              :value 50
              :error [nil nil]
              :logs [{:indent 0
                      :time (:time (nth (:logs result-bucket) 0))
                      :level :info
                      :value "Level 1: Begin nested processing"}
                     {:indent 4
                      :time (:time (nth (:logs result-bucket) 1))
                      :level :info
                      :value "Level 2: Starting with 5"}
                     {:indent 8
                      :time (:time (nth (:logs result-bucket) 2))
                      :level :info
                      :value "Level 3: Processing 5"}
                     {:indent 4
                      :time (:time (nth (:logs result-bucket) 3))
                      :level :info
                      :value "Level 2: Got result 15"}
                     {:indent 0
                      :time (:time (nth (:logs result-bucket) 4))
                      :level :info
                      :value "Level 1: Intermediate result is 25"}
                     {:indent 0
                      :time (:time (nth (:logs result-bucket) 5))
                      :level :info
                      :value "Level 1: Final result is 50"}]}
             result-bucket))
      "Complete bucket with result 50 and six log entries with automatic depth-based indentation")))

(deftest redirect-stdout-custom-spacing
  (testing "spacing option controls indentation step"
    (let [lifted (bucket/bucketize plain-fn-nested)
          f (wrap/redirect-stdout lifted :spacing 3 :mode :depth-aware)
          resp (f (bucket/grab 5 :logs []))
          [log1 log2 log3 log4 log5 log6] (:logs resp)
          expected {:id (:id resp)
                    :name (:name resp)
                    :meta (:meta resp)
                    :value (:value resp)
                    :error [nil nil]
                    :logs [(assoc log1 :indent 0)
                           (assoc log2 :indent 6)
                           (assoc log3 :indent 12)
                           (assoc log4 :indent 6)
                           (assoc log5 :indent 0)
                           (assoc log6 :indent 0)]}]
      (is (= expected (bucket-view resp))))))
