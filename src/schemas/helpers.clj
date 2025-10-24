(ns schemas.helpers
  (:require [malli.core :as m]))

(defn valid?
  "Check if value matches schema"
  [schema value]
  (m/validate schema value))

(defn explain
  "Explain why value doesn't match schema"
  [schema value]
  (m/explain schema value))

(defn validate-args
  "Decorator that validates function arguments against a schema.

   Usage:
   (def my-fn
     (validate-args SomeOpts
       (fn [opts] ...)))

   Throws ex-info if arguments don't match schema."
  [schema f]
  (fn [args]
    (if (valid? schema args)
      (f args)
      (throw (ex-info "Invalid arguments"
                      {:schema (str schema)
                       :args args
                       :explanation (str (explain schema args))})))))

(defn validate-return
  "Decorator that validates function return value against a schema.

   Usage:
   (def my-fn
     (validate-return Bucket
       (fn [opts] ...)))

   Throws ex-info if return value doesn't match schema."
  [schema f]
  (fn [args]
    (let [result (f args)]
      (if (valid? schema result)
        result
        (throw (ex-info "Invalid return value"
                        {:schema (str schema)
                         :result result
                         :explanation (str (explain schema result))}))))))

(defn validate-fn
  "Decorator that validates both arguments and return value.

   Usage:
   (def my-fn
     (validate-fn {:args SomeOpts :ret Bucket}
       (fn [opts] ...)))"
  [{:keys [args ret]} f]
  (let [f' (if args (validate-args args f) f)
        f'' (if ret (validate-return ret f') f')]
    f''))

