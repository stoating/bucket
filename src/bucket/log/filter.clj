(ns bucket.log.filter
  (:refer-clojure :exclude [time])
  (:require [bin.time :as time])
  (:import [java.time Instant]))

(def ^:private level-order
  {:debug 0 :info 1 :warning 2 :error 3 :critical 4})

(defn level
  [logs type value]
  (let [target (level-order value 0)]
    (filterv (fn [{:keys [level]}]
               (let [lvl (level-order level 0)]
                 (case type
                   :lte (<= lvl target)
                   :gte (>= lvl target)
                   :eq (= lvl target)
                   false)))
             logs)))

(defn indent
  [logs type value]
  (let [target (if (number? value) value 0)]
    (filterv (fn [{:keys [indent]}]
               (let [indent (or indent 0)]
                 (case type
                   :lte (<= indent target)
                   :gte (>= indent target)
                   :eq (= indent target)
                   false)))
             logs)))

(defn time
  [logs type value]
  (let [comparison-ms (or (time/->millis value)
                          (inst-ms (Instant/now)))]
    (filterv (fn [{:keys [time]}]
               (let [entry-ms (time/->millis time)]
                 (when entry-ms
                   (case type
                     :lte (<= entry-ms comparison-ms)
                     :gte (>= entry-ms comparison-ms)
                     :eq (= entry-ms comparison-ms)
                     false))))
             logs)))

(defn value
  [logs type value]
  (let [pattern (if (instance? java.util.regex.Pattern value)
                  value
                  (re-pattern (str value)))]
    (filterv (fn [{:keys [value]}]
               (let [matches? (some? (and value (re-find pattern value)))]
                 (case type
                   :eq matches?
                   :neq (not matches?)
                   false)))
             logs)))