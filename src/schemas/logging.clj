(ns schemas.logging
  "Schemas for logging functionality")

(def LogLevel
  "Schema for log levels

  ```clojure
  [:enum :debug :info :warning :error :critical]
  ```"
  [:enum :debug :info :warning :error :critical])

(def LogEntry
  "Schema for a single log entry

  ```clojure
  [:map
   [:indent [:int {:min 0}]]
   [:time :int]
   [:level LogLevel]
   [:value :string]]
  ```"
  [:map
   [:indent [:int {:min 0}]]
   [:time :int]
   [:level LogLevel]
   [:value :string]])

(def Logs
  "Schema for a vector of log entries

  ```clojure
  [:vector LogEntry]
  ```"
  [:vector LogEntry])
