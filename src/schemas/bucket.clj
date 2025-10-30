;; Bucket-specific schemas
(ns schemas.bucket
  "Schemas for Bucket operations and function types"
  (:require [clj-ulid :as ulid]
            [schemas.error :as es]
            [schemas.logging :as ls]))

(def Bucket
  "Schema for the Bucket monad container

  ```clojure
  [:map
   [:id {:default (fn [] (ulid))} string?]
   [:name {:default (fn [bucket] (str (:id bucket) \"-bucket\"))} string?]
   [:meta {:default {}} map?]
   [:error es/ErrorTuple]
   [:logs ls/Logs]
   [:value :any]]
  ```"
  [:map
   [:id {:default (fn [] (ulid/ulid))} string?]
   [:name {:default (fn [bucket] (str (:id bucket) "-bucket"))} string?]
   [:meta {:default {}} map?]
   [:error es/ErrorTuple]
   [:logs ls/Logs]
   [:value :any]])

(def BucketFunction
  "Schema for a function that takes a value and returns a Bucket

  ```clojure
  [:=> [:cat :any] Bucket]
  ```"
  [:=> [:cat :any] Bucket])

(def PureFunction
  "Schema for a pure function that transforms values

  ```clojure
  [:=> [:cat :any] :any]
  ```"
  [:=> [:cat :any] :any])

(def BucketSequence
  "Schema for a sequence of Bucket values

  ```clojure
  [:sequential Bucket]
  ```"
  [:sequential Bucket])
