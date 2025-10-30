(ns monad
  "Monadic operations for the Bucket type.

   The Bucket type forms a monad where:
   - The Bucket map {:id \"...\" :name \"...\" :meta {} :error [...] :logs [...] :value ...} is the container
   - Computations can fail (error handling) and accumulate context (logs)
   - The monad handles log accumulation and error short-circuiting automatically"
  (:require [schemas.bucket :as bs]
            [clj-ulid :as ulid]))

(defn pure
  "Wrap a plain value in the Bucket monad (monadic return/pure).

   Creates a Bucket with the given value and optional logs. Optionally accepts
   an explicit error tuple to construct a fail-bucket immediately (use sparingly).
   Supports optional metadata and custom naming via keyword arguments.

   Type: () -> Bucket
         | (Any) -> Bucket
         | (:value Any, :logs Logs, :error ErrorTuple, :meta Map, :name String) -> Bucket

   Schema:
   ```clojure
   [:=> [:cat [:* [:cat :keyword :any]]] Bucket]
   ```

   Args:
   Zero-arity form:
   - Creates an empty bucket with nil value

   Single-arity form (for monad composition):
   - value - the value to wrap (any type)

   Keyword arguments form (all optional):
   - :value - the value to wrap (any type, default nil)
   - :logs - optional vector of log entries (default [])
             Each log entry: {:indent int :time inst :level keyword :value string}
   - :error - optional error tuple [exception-or-nil string-or-nil] (default [nil nil])
   - :meta - optional metadata map to attach to the bucket (default {})
   - :name - optional custom name string for the bucket (default \"<uuid>-bucket\")

   Returns: Bucket containing the value and provided logs/error/meta/name.

   Examples:
   ```clojure
   ;; Zero-arity usage
   (pure)  ; => Bucket with nil value

   ;; Single-arity usage (for monad laws and composition)
   (pure 42)
   (pure {:a 1})
   (pure [1 2 3])

   ;; Keyword argument usage
   (pure :value 42)
   (pure :value :my-keyword)
   (pure :value 42 :logs [log1 log2])

   ;; With metadata only
   (pure 42 :meta {:source :api})

   ;; With name only
   (pure 42 :name \"api-result\")

   ;; With both metadata and name
   (pure 42 :meta {:source :api} :name \"api-result\")

   ;; With logs and options
   (pure 42 :logs [log1] :meta {:source :api})
   (pure 42 :logs [log1] :name \"api-result\")
   (pure 42 :logs [log1] :meta {:source :api} :name \"api-result\")

   ;; With logs, error, and options
   (pure 42 :logs [log1] :error [ex \"error\"] :meta {:source :api})
   (pure 42 :logs [log1] :error [ex \"error\"] :name \"api-result\")
   (pure 42 :logs [log1] :error [ex \"error\"] :meta {:source :api} :name \"api-result\")

   ;; Value as first argument (without :value keyword) - odd number of args
   (pure 42 :logs [log1])
   (pure \"result\" :meta {:source :api})
   (pure [1 2 3] :logs [log1] :name \"my-bucket\")
   ```"
  {:malli/schema [:=> [:cat [:* [:cat :keyword :any]]] bs/Bucket]}
  ([]
   (pure :value nil))
  ([value]
   (pure :value value))
  ([a b & rest]
   (let [id (ulid/ulid)
         args (if (odd? (+ 2 (count rest)))
                (apply hash-map :value a b rest)
                (apply hash-map a b rest))
         {:keys [value logs error meta name]} args]
     {:id id
      :name (or name (str id "-bucket"))
      :meta (or meta {})
      :value value
      :logs (or logs [])
      :error (or error [nil nil])})))

(defn bind
  "Chain Bucket computations (monadic bind/flatMap).

   If the Bucket contains an error, returns it unchanged (short-circuit).
   Otherwise, applies the function to the :value value and merges logs.

   Type: (Bucket, (Any -> Bucket)) -> Bucket
   Schema:
   ```clojure
   [:=> [:cat Bucket ifn?] Bucket]
   ```

   Args:
   - resp: Bucket map
   - f: function that takes a value and returns a Bucket

   Returns: Bucket with accumulated logs and computed value

   Examples:
   (bind (pure 5) #(pure (* % 2)))          ; => Bucket with value 10
   (bind (failure [err]) identity)          ; => unchanged failure
   (bind (pure 5) #(pure % (log \"step\"))) ; => accumulates logs"
  {:malli/schema [:=> [:cat bs/Bucket ifn?] bs/Bucket]}
  [bucket f]
  (let [{:keys [id name meta error logs value]} bucket]
    (if (first error) ; short-circuit on error
      bucket
      (let [new-bucket (f value)
            combined-logs (into logs (:logs new-bucket))]
        (-> new-bucket
            (assoc :logs combined-logs)
            (assoc :id id)
            (assoc :name name)
            (assoc :meta meta))))))

(defn fmap
  "Apply a function to the value inside a Bucket (monadic fmap).

   If the Bucket contains an error, returns it unchanged.
   Otherwise, applies the function to transform the :value value.
   Logs are preserved unchanged.

   Type: ((Any -> Any), Bucket) -> Bucket
   Schema:
   ```clojure
   [:=> [:cat ifn? Bucket] Bucket]
   ```

   Args:
   - f: function to transform the value (a -> b)
   - resp: Bucket map

   Returns: Bucket with transformed value

   Examples:
   (fmap inc (pure 5))           ; => Bucket with value 6
   (fmap str (pure 42))          ; => Bucket with value \"42\"
   (fmap inc (failure [err]))    ; => unchanged failure"
  {:malli/schema [:=> [:cat ifn? bs/Bucket] bs/Bucket]}
  [f resp]
  (if (first (:error resp))
    resp
    (update resp :value f)))

(defn join
  "Flatten nested Bucket containers (monadic join).

   Converts Bucket (Bucket a) -> Bucket a by flattening one level
   and accumulating logs from both layers.

   Type: Bucket -> Bucket
   Schema:
   ```clojure
   [:=> [:cat Bucket] Bucket]
   ```

   Args:
   - resp: Bucket containing another Bucket

   Returns: flattened Bucket with accumulated logs

   Examples:
   (join (pure (pure 42)))       ; => Bucket with value 42
   (join (pure (failure [err]))) ; => failure with accumulated logs"
  {:malli/schema [:=> [:cat bs/Bucket] bs/Bucket]}
  [resp]
  (let [{:keys [id name meta error logs value]} resp]
    (if (first error)
      resp
      (if (map? value)
        (-> value
            (update :logs (fn [log] (into logs log)))
            (assoc :id id)
            (assoc :name name)
            (assoc :meta meta))
        (throw (ex-info "Cannot join - inner value is not a Bucket"
                        {:value value}))))))

(defn lift
  "Lift a regular function into the Bucket monad.

   Takes a function (a -> b) and returns a function (Bucket a -> Bucket b).
   This is equivalent to (partial fmap f).

   Type: (Any -> Any) -> (Bucket -> Bucket)
   Schema:
   ```clojure
   [:=> [:cat ifn?] ifn?]
   ```

   Args:
   - f: regular function to lift

   Returns: function that operates on Bucket values

   Examples:
   (def lifted-inc (lift inc))
   (lifted-inc (pure 5))         ; => Bucket with value 6"
  {:malli/schema [:=> [:cat ifn?] ifn?]}
  [f]
  (partial fmap f))

;; Monadic operators and combinators

(defn >>
  "Left-to-right monadic composition (Kleisli composition).

   Composes monadic functions: (a -> Bucket b) -> (b -> Bucket c) -> (a -> Bucket c)

   Type: (Any -> Bucket) -> (Any -> Bucket) -> (Any -> Bucket)
   Schema:
   ```clojure
   [:=> [:cat ifn? ifn?]
        ifn?]
   ```

   Usage: ((>> f g) x) is equivalent to (bind (f x) g)"
  {:malli/schema [:=> [:cat ifn? ifn?] ifn?]}
  [f g]
  (fn [x] (bind (f x) g)))

(defn sequence-m
  "Convert a sequence of Bucket values into a Bucket of sequence.

   If any Bucket contains an error, returns the first error.
   Otherwise returns a Bucket containing a vector of all values.
   All logs are accumulated.

   Type: [Bucket] -> Bucket
   Schema:
   ```clojure
   [:=> [:cat [:sequential Bucket]]
        Bucket]
   ```

   Args:
   - responses: sequence of Bucket values

   Returns: Bucket containing vector of values, or first error

   Examples:
   (sequence-m [(pure 1) (pure 2)])  ; => Bucket with value [1 2]
   (sequence-m [(pure 1) (failure [err])]) ; => failure"
  {:malli/schema [:=> [:cat [:sequential bs/Bucket]] bs/Bucket]}
  [responses]
  (reduce (fn [acc resp]
            (bind acc (fn [values]
                        (fmap (fn [value] (conj values value)) resp))))
          (pure :value [])
          responses))

(defn map-m
  "Map a monadic function over a sequence.

   Equivalent to (sequence-m (map f xs)).

   Type: (Any -> Bucket) -> [Any] -> Bucket
   Schema:
   ```clojure
   [:=> [:cat ifn? sequential?] Bucket]
   ```

   Args:
   - f: function that returns Bucket values
   - xs: sequence to map over

   Returns: Bucket containing vector of values"
  {:malli/schema [:=> [:cat ifn? sequential?] bs/Bucket]}
  [f xs]
  (sequence-m (map f xs)))
