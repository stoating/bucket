# 🪣 Bucket

A useful, beginner-friendly monad.

Instead of talking about monads, functors, and applicatives, we talk about **buckets**. You **grab** a bucket, **pass** it around, **stir** things into it, and eventually **spill** out the results.

## Why Bucket?

Traditional monad libraries can be intimidating with their academic terminology. Bucket makes functional programming patterns accessible by using intuitive, everyday metaphors:

- ✅ **Automatic error handling** - errors short-circuit the computation chain
- ✅ **Log accumulation** - track what happened at each step
- ✅ **Metadata support** - attach context to your computations
- ✅ **Composable operations** - chain transformations naturally
- ✅ **Beginner-friendly API** - no PhD required

## Quick Start

```clojure
(require '[bucket :as bucket])

;; Grab a fresh bucket with a value
(def my-bucket (bucket/grab 42))

;; Pass the bucket through a series of transformations
(-> (bucket/grab 5)
    (bucket/pass (fn [x] (bucket/grab (* x 2))))  ; 10
    (bucket/pass (fn [x] (bucket/grab (+ x 3))))  ; 13
    (bucket/stir-in inc))                         ; 14
;; => Bucket with result: 14

;; Errors automatically short-circuit
(-> (bucket/grab 5)
    (bucket/pass (fn [_] (bucket/grab :error [(Exception. "Oops!") "Context"])))
    (bucket/pass (fn [x] (bucket/grab (* x 2))))) ; This never runs!
;; => Bucket with error

;; Spill the bucket to get the final result
(require '[bucket.spouts :as spouts])
(spouts/spill my-bucket)
;; Prints logs, handles errors, returns result
```

## Core Concepts

### The Bucket Structure

A Bucket is a map containing:

```clojure
{:id "01HMXYZ..."       ; Unique ULID identifier
 :name "01HMXYZ-bucket" ; Human-readable name
 :meta {}               ; Metadata map
 :result 42             ; Your actual value (can be anything)
 :logs []               ; Vector of log entries
 :error [nil nil]}      ; Error tuple [exception message]
```

### Core Operations

#### `grab` - Creating Buckets

Grab a fresh bucket, optionally with a value, logs, metadata, or even an error:

```clojure
;; Empty bucket
(bucket/grab)

;; Bucket with a value
(bucket/grab 42)

;; Bucket with logs
(bucket/grab "data" :logs [{:indent 0 :time (Instant/now) :level :info :value "Starting"}])

;; Bucket with metadata
(bucket/grab 42 :meta {:source :api :version 1})

;; Bucket with custom name
(bucket/grab 42 :name "my-calculation")

;; Combine all the options
(bucket/grab 42
  :logs [...]
  :meta {:user "alice"}
  :name "api-result")
```

#### `pass` - Chaining Computations

Pass the bucket to the next function. If there's an error, it short-circuits:

```clojure
(bucket/pass my-bucket
  (fn [value]
    (bucket/grab (* value 2))))
```

Use threading for clean chains:

```clojure
(-> (bucket/grab 10)
    (bucket/pass (fn [x] (bucket/grab (+ x 5))))
    (bucket/pass (fn [x] (bucket/grab (* x 2))))
    (bucket/pass (fn [x] (bucket/grab (str "Result: " x)))))
```

#### `stir-in` - Transforming Values

Apply a pure function to the value inside the bucket:

```clojure
(-> (bucket/grab "hello")
    (bucket/stir-in clojure.string/upper-case)
    (bucket/stir-in #(str % "!")))
;; => Bucket with result: "HELLO!"
```

#### `gather` - Collecting Multiple Buckets

Gather multiple buckets into one bucket containing a vector of results:

```clojure
(bucket/gather [(bucket/grab 1)
                (bucket/grab 2)
                (bucket/grab 3)])
;; => Bucket with result: [1 2 3]

;; Errors short-circuit
(bucket/gather [(bucket/grab 1)
                (bucket/grab :error [(Exception. "Oops!") "msg"])
                (bucket/grab 3)])
;; => Bucket with error (third bucket never evaluated)
```

#### `gather-with` - Map and Gather

Apply a function to each item and gather the results:

```clojure
(bucket/gather-with
  (fn [x] (bucket/grab (* x 2)))
  [1 2 3 4 5])
;; => Bucket with result: [2 4 6 8 10]
```

#### `consolidate` - Flattening Nested Buckets

When you have a bucket containing another bucket, flatten it:

```clojure
(-> (bucket/grab 42)
    (bucket/pass (fn [x] (bucket/grab (bucket/grab (* x 2)))))
    (bucket/consolidate))
;; => Bucket with result: 84
;; (Inner bucket's logs are merged with outer bucket's logs)
```

#### `bucketize` - Lifting Regular Functions

Turn any regular function into one that works with buckets:

```clojure
(def bucket-inc (bucket/bucketize inc))
(bucket-inc (bucket/grab 41))
;; => Bucket with result: 42
```

### Spouts - Getting Data Out

Spouts are utilities for extracting data from buckets or combining them.

#### `spill` - The Main Output Function

Spill your bucket to get the result, print logs, and handle errors:

```clojure
(require '[bucket.spouts :as spouts])

;; Basic usage - returns the result
(spouts/spill my-bucket)

;; Control where logs go
(spouts/spill my-bucket :log-out :stdout)  ; Print to console
(spouts/spill my-bucket :log-out :file)    ; Write to file
(spouts/spill my-bucket :log-out :both)    ; Both!
(spouts/spill my-bucket :log-out :none)    ; Silent

;; Require a non-nil result
(spouts/spill my-bucket :require-result true)  ; Fails if result is nil

;; Custom output directory
(spouts/spill my-bucket :out-dir "my-logs")
```

#### `drain-*` - Extracting Specific Fields

```clojure
(spouts/drain-result my-bucket)   ; Get just the result
(spouts/drain-logs my-bucket)     ; Get just the logs
(spouts/drain-error my-bucket)    ; Get just the error
(spouts/drain-id my-bucket)       ; Get the ID
(spouts/drain-name my-bucket)     ; Get the name
(spouts/drain-meta my-bucket)     ; Get the metadata
```

#### `pour-into` - Combining Buckets

Pour one bucket into another, combining their contents:

```clojure
(require '[bucket.spouts :as spouts])

;; Gather results into a vector (default)
(spouts/pour-into new-bucket old-bucket)
;; => Bucket with result: [old-result new-result]

;; Different pour types
(spouts/pour-into new-bucket old-bucket :pour-type :drop-old)
;; => Uses new-result only

(spouts/pour-into new-bucket old-bucket :pour-type :drop-new)
;; => Uses old-result only

(spouts/pour-into new-bucket old-bucket :pour-type :stir-in-old->new)
;; => Applies old-result as function to new bucket

(spouts/pour-into new-bucket old-bucket :pour-type :stir-in-new->old)
;; => Applies new-result as function to old bucket
```

#### `serialize-bucket` - Serialization

Save bucket state to JSON or EDN:

```clojure
(require '[bucket.spouts.transform :as transform])

;; To stdout as EDN
(transform/serialize-bucket my-bucket)

;; To file as JSON
(transform/serialize-bucket my-bucket
  :format :json
  :out :file
  :dir "output"
  :name "my-bucket")
```

#### `summarize` - Quick Overview

Get a human-readable summary:

```clojure
(transform/summarize my-bucket)
;; Returns string like:
;; "Bucket 01HMXYZ: OK
;;  Result: 42
;;  Logs: 3 entries
;;  No errors"
```

### Logging

The primary way to add logs to a bucket is with the `log` function, which appends log entries to a log vector:

```clojure
(require '[bucket.logging :as logging])

;; Start with an empty log vector
(def logs [])

;; Add a basic log entry (defaults to :info level)
(def logs (logging/log logs "Processing started"))

;; Add logs with different levels
(def logs (-> logs
              (logging/log "Debug information" :level :debug)
              (logging/log "Important info" :level :info)
              (logging/log "Be careful!" :level :warning)
              (logging/log "Something failed" :level :error)
              (logging/log "System down!" :level :critical)))

;; Add indented logs
(def logs (-> logs
              (logging/log "Main step")
              (logging/log "Sub-step 1" :indent 2)
              (logging/log "Sub-step 2" :indent 2)))

;; Indent automatically inherits from previous entry
(def logs (-> logs
              (logging/log "Parent operation" :indent 1)
              (logging/log "Child operation")))  ; Automatically indented at level 1

;; Redact sensitive information (warning: not foolproof)
(def logs (logging/log logs
                       "password=secret123"
                       :check-pass true))
;; => Logs "* log redacted *" instead of the actual message
```

#### Using logs with buckets

Logs are typically added when creating buckets:

```clojure
(require '[bucket :as b])

;; Create a bucket with logs
(def my-bucket
  (b/grab {:status "ok"}
    :logs (-> []
              (logging/log "Started processing")
              (logging/log "Validated input" :indent 2)
              (logging/log "Processing complete"))))

;; Add logs when passing buckets
(b/pass my-bucket
  (fn [data]
    (b/grab (assoc data :processed true)
      :logs (logging/log [] "Data processed" :level :info))))
```

#### Automatic password detection

When `:check-pass true` is enabled, the `log` function automatically redacts messages that appear to contain sensitive information like passwords, API keys, tokens, and other secrets.

### Wrappers - Decorating Functions

Wrappers add functionality to functions automatically:

```clojure
(require '[bucket.wrap :as wrap])

;; Catch exceptions automatically
(def safe-fn
  (wrap/wrap my-risky-function
    :catch-error true))

;; Log function calls
(def logged-fn
  (wrap/wrap my-function
    :log-function true
    :log-function-level :info))

;; Log arguments (with redaction)
(def arg-logged-fn
  (wrap/wrap my-function
    :log-args true
    :redact-logs true))  ; Redacts passwords/secrets

;; Capture stdout
(def capturing-fn
  (wrap/wrap my-function
    :redirect-stdout true))

;; Combine multiple wrappers
(def super-safe-fn
  (wrap/wrap my-function
    :catch-error true
    :log-function true
    :log-args true
    :redirect-stdout true))
```

## Repository Structure

``` bash
bucket/
├── src/
│   ├── bucket.clj                    # Main API - grab, pass, stir-in, etc.
│   ├── monad.clj                     # Core monad implementation
│   ├── bucket/
│   │   ├── error.clj                 # Error handling utilities
│   │   ├── logging.clj               # Logging utilities
│   │   ├── spouts.clj                # End-user spout helpers
│   │   ├── wrap.clj                  # Function wrapper combinators
│   │   ├── spouts/
│   │   │   ├── extract.clj           # spill, drain-* functions
│   │   │   ├── chain.clj             # pour-into helpers
│   │   │   ├── transform.clj         # serialize-bucket, summarize
│   │   │   └── aggregate.clj         # collect-metrics, merge-into
│   │   └── wraps/
│   │       ├── catch_error.clj       # Exception catching wrapper
│   │       ├── log_args.clj          # Argument logging wrapper
│   │       ├── log_function.clj      # Function call logging wrapper
│   │       └── redirect_stdout.clj   # Stdout capture wrapper
│   └── schemas/
│       ├── bucket.clj                # Malli schemas for Bucket types
│       ├── error.clj                 # Error schemas
│       ├── logging.clj               # Logging schemas
│       └── helpers.clj               # Schema utilities
│
├── test/
│   └── bucket/
│       ├── *_test.clj                # Comprehensive test suite
│       ├── combinators/              # Tests for gather, consolidate, etc.
│       ├── spouts/                   # Tests for spill, pour-into, etc.
│       └── wrap/                     # Tests for wrapper functions
│
├── deps.edn                          # Clojure dependencies
└── README.md                         # This file
```

## Examples

### Example 1: Data Pipeline with Error Handling

```clojure
(require '[bucket :as b]
         '[bucket.spouts :as spouts])

(defn fetch-user [id]
  (if (pos? id)
    (b/grab {:id id :name "Alice"}
      :logs [(logging/make-entry (str "Fetched user " id))])
    (b/grab :error [(Exception. "Invalid ID") "ID must be positive"])))

(defn enrich-user [user]
  (b/grab (assoc user :enriched true)
    :logs [(logging/make-entry "Enriched user data")]))

(defn format-user [user]
  (b/grab (str "User: " (:name user))
    :logs [(logging/make-entry "Formatted user")]))

;; Success case
(-> (fetch-user 42)
    (b/pass enrich-user)
    (b/pass format-user)
    (spouts/spill))
;; Prints all logs, returns: "User: Alice"

;; Error case - short-circuits after fetch
(-> (fetch-user -1)
    (b/pass enrich-user)      ; Never runs
    (b/pass format-user)      ; Never runs
    (spouts/spill :exit :fail))
;; Prints error, exits with code 1
```

### Example 2: Parallel Processing with gather

```clojure
(defn process-item [item]
  (b/grab (* item 2)
    :logs [(logging/make-entry (str "Processed " item))]))

;; Process multiple items
(->> [1 2 3 4 5]
     (b/gather-with process-item)
     (spouts/spill))
;; Returns: [2 4 6 8 10]
;; Logs show all processing steps
```

### Example 3: Complex Computation with Metadata

```clojure
(-> (b/grab {:amount 100 :currency "USD"}
      :meta {:user "alice" :request-id "xyz-123"})
    (b/pass (fn [data]
              (b/grab (update data :amount * 1.2)
                :logs [(logging/make-entry "Applied tax")])))
    (b/pass (fn [data]
              (b/grab (assoc data :formatted
                        (str (:currency data) " " (:amount data)))
                :logs [(logging/make-entry "Formatted output")])))
    (spouts/spill :meta-out :both))
;; Result: {:amount 120 :currency "USD" :formatted "USD 120"}
;; Metadata preserved throughout
;; Logs written to both stdout and file
```

## Installation

### As a Git Dependency

Add to your `deps.edn`:

```clojure
{:deps {io.github.stoating/bucket
        {:git/url "https://github.com/stoating/bucket"
         :git/sha "..."}}}  ; Use latest commit SHA
```

### Fork and Experiment

1. **Clone the repository:**

   ```bash
   git clone https://github.com/stoating/bucket.git
   cd bucket
   ```

2. **Explore the tests:**

   ```bash
   clj -M:test
   ```

3. **Play in the REPL:**

   ```bash
   clj -M:dev
   ```

   ```clojure
   (require '[bucket :as b])
   (b/grab 42)
   ```

4. **Read the tests** - The `test/` directory contains comprehensive examples showing every feature in action

## Why "Bucket"?

Academic monad terminology can be intimidating:

- **return/pure** → **grab** (grab a bucket)
- **bind/flatMap** → **pass** (pass the bucket along)
- **fmap/map** → **stir-in** (stir something into the bucket)
- **join/flatten** → **consolidate** (consolidate nested buckets)
- **sequence** → **gather** (gather multiple buckets)

We prefer metaphors that make sense: you grab a bucket, pass it around, stir things into it, pour it into other buckets, and eventually spill out the contents. Much more intuitive than "monadic bind operations"!

## Testing

The library includes an extensive test suite with over 50 test namespaces covering:

- Core operations (grab, pass, stir-in)
- Combinators (gather, gather-with, consolidate)
- Spouts (spill, pour-into, drain-*)
- Error handling
- Logging
- Wrappers
- Serialization

Run tests with:

```bash
clj -M:test
```

## Schema Validation

Bucket uses [Malli](https://github.com/metosin/malli) for runtime schema validation. All core functions are annotated with schemas to help catch errors early.

## Contributing

Contributions welcome! Whether you:

- Found a bug
- Have a feature idea
- Want to improve documentation
- Want to add more tests

Feel free to open an issue or pull request.

## License

MIT License - See LICENSE file for details.

Copyright (c) 2025 stoating

---

## Happy Bucketing! 🪣

*Remember: A bucket a day keeps the imperative code away.*
