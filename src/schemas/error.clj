;; Error schemas
(ns schemas.error
  "Schemas for error handling")

(def ErrorTuple
  "Schema for error tuple: [exception-or-nil string-or-nil]

  ```clojure
  [:tuple
   [:maybe [:or :string
            [:fn #(instance? Throwable %)]]]
   [:maybe :string]]
  ```"
  [:tuple
   [:maybe [:or :string
            [:fn #(instance? Throwable %)]]]
   [:maybe :string]])
