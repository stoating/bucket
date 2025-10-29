(ns bucket
  "A Bucket contains:
   - :id     - a unique identifier string (ULID)
   - :name   - a name string (default: \"<id>-bucket\")
   - :meta   - a metadata map (default: {})
   - :error  - a tuple/vector [exception-or-nil string-or-nil]
   - :logs   - a vector of log entries [indent timestamp level message]
   - :result - any value or nil

   Main helpers:
   - pass       - chain Bucket computations (delegates to monad/bind)
   - pass->     - compose monadic functions (delegates to monad/>>)
   - stir-in    - apply a pure transform to :result (delegates to monad/fmap)
   - consolidate- flatten nested Bucket one level (delegates to monad/join)
   - gather     - collect [Bucket a] into Bucket [a] (delegates to monad/sequence-m)
   - gather-with- map a monadic fn over items and gather (delegates to monad/map-m)
   - bucketize  - lift a plain function to operate on Buckets (delegates to monad/lift)
   - grab       - construct a fresh Bucket with optional initial values (delegates to monad/pure)

   Output helpers:
   - spill      (fb.core.bucket.spouts) - prints logs, handles error, returns :result; optionally fails on nil
   - pour-into  (fb.core.bucket.spouts) - appends logs to external log vector; optionally fails if :result is nil"
  (:require
   [monad :as monad]
   [schemas.bucket :as bs]))

(defn pass
  "Pass the bucket to the next stage (alias of monad/bind).

   Schema:
   ```clojure
   [:=> [:cat Bucket BucketFunction] Bucket]
   ```

   Chains computations that return Bucket values while accumulating logs and
   short-circuiting on error.

   Args:
   - bucket: Bucket map
   - f: function (value -> Bucket)

   Returns: Bucket"
  {:malli/schema [:=> [:cat bs/Bucket bs/BucketFunction] bs/Bucket]}
  [bucket f]
  (monad/bind bucket f))

(defn stir-in
  "Stir a function into the bucket contents (alias of monad/fmap).

   Schema:
   ```clojure
   [:=> [:cat PureFunction Bucket] Bucket]
   ```

   Applies a pure function to the :result value when no error is present.
   Preserves logs unchanged and short-circuits on error.

   Args:
   - f: (value -> new-value)
   - bucket: Bucket map

   Returns: Bucket with transformed :result"
  {:malli/schema [:=> [:cat bs/PureFunction bs/Bucket] bs/Bucket]}
  [f bucket]
  (monad/fmap f bucket))

(defn consolidate
  "Flatten a nested Bucket one level (alias of monad/join).

   Schema:
   ```clojure
   [:=> [:cat Bucket] Bucket]
   ```

   Takes a Bucket whose :result is itself a Bucket, merges logs from both
   levels, and returns a single-level Bucket. Short-circuits on outer error.

   Args:
   - bucket: Bucket map, possibly nested in :result

   Returns: flattened Bucket"
  {:malli/schema [:=> [:cat bs/Bucket] bs/Bucket]}
  [bucket]
  (monad/join bucket))

(defn bucketize
  "Lift a plain function to operate on Buckets (alias of monad/lift).

   Schema:
   ```clojure
   [:=> [:cat PureFunction] [:=> [:cat Bucket] Bucket]]
   ```

   Takes a function f: (a -> b) and returns a function g: (Bucket a -> Bucket b)
   that applies f to the :result when no error is present, preserving logs.

   Args:
   - f: plain function

   Returns: lifted function that accepts/returns Bucket values"
  {:malli/schema [:=> [:cat bs/PureFunction] [:=> [:cat bs/Bucket] bs/Bucket]]}
  [f]
  (monad/lift f))

(defn pass->
  "Compose two monadic functions (alias of monad/>>).

   Schema:
   ```clojure
   [:=> [:cat BucketFunction BucketFunction] BucketFunction]
   ```

   Given f: a -> Bucket b and g: b -> Bucket c, returns h: a -> Bucket c that
   runs f then passes its result to g, preserving error short-circuiting and logs.

   Args:
   - f: monadic function (a -> Bucket b)
   - g: monadic function (b -> Bucket c)

   Returns: composed monadic function (a -> Bucket c)"
  {:malli/schema [:=> [:cat bs/BucketFunction bs/BucketFunction] bs/BucketFunction]}
  [f g]
  (monad/>> f g))

(defn gather
  "Collect a sequence of Bucket values into a single Bucket of a vector of results.

   Schema:
   ```clojure
   [:=> [:cat BucketSequence] Bucket]
   ```

   Alias of monad/sequence-m. Accumulates logs and short-circuits on the first error.

   Args:
   - buckets: sequence of Bucket values

   Returns: Bucket with :result vector of results or the first error encountered"
  {:malli/schema [:=> [:cat bs/BucketSequence] bs/Bucket]}
  [buckets]
  (monad/sequence-m buckets))

(defn gather-with
  "Map a monadic function over a sequence and gather results (alias of monad/map-m).

   Schema:
   ```clojure
   [:=> [:cat BucketFunction [:sequential :any]] Bucket]
   ```

   Equivalent to (gather (map f xs)). Accumulates logs and short-circuits on first error.

   Args:
   - f: function (a -> Bucket b)
   - xs: sequence of input values

   Returns: Bucket with :result vector of transformed values"
  {:malli/schema [:=> [:cat bs/BucketFunction [:sequential :any]] bs/Bucket]}
  [f xs]
  (monad/map-m f xs))

(defn grab
  "Grabs a fresh Bucket. Optionally put stuff in the Bucket. (alias of monad/pure).
   Supports optional metadata and custom naming via keyword arguments.

   Schema:
   ```clojure
   [:=> [:cat [:* [:cat :keyword :any]]] Bucket]
   ```

   Args (all keyword arguments):
   - :value - the value to wrap (any type, default nil)
   - :logs - optional vector of log entries (default [])
   - :error - optional error tuple [exception-or-nil string-or-nil] (default [nil nil])
   - :meta - optional metadata map to attach to the bucket (default {})
   - :name - optional custom name string for the bucket (default \"<uuid>-bucket\")

   Returns: Bucket map. Delegates to monad/pure.

   Examples:
   ```clojure
   ;; Basic usage
   (grab)
   (grab 42)
   (grab :value :my-keyword)
   (grab :value 42 :logs [log1 log2])

   ;; With metadata only
   (grab :value 42 :meta {:source :api})

   ;; With name only
   (grab 42 :name \"api-result\")

   ;; With both metadata and name
   (grab 42 :meta {:source :api} :name \"api-result\")

   ;; With logs and options
   (grab 42 :logs [log1] :meta {:source :api})
   (grab 42 :logs [log1] :name \"api-result\")
   (grab 42 :logs [log1] :meta {:source :api} :name \"api-result\")

   ;; With logs, error, and options
   (grab 42 :logs [log1] :error [ex \"error\"] :meta {:source :api})
   (grab 42 :logs [log1] :error [ex \"error\"] :name \"api-result\")
   (grab 42 :logs [log1] :error [ex \"error\"] :meta {:source :api} :name \"api-result\")
   ```"
  {:malli/schema [:=> [:cat [:* [:cat :keyword :any]]] bs/Bucket]}
  [& args]
  (apply monad/pure args))
