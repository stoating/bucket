(ns bucket.wraps.wrapper.redirect-stdout
  (:require [bin.defaults :as default]
            [bucket.log :as log]
            [clojure.math :as math]
            [clojure.string :as str]))

(def ^:private default-excluded-prefixes
  ["java." "javax." "sun." "jdk."
   "clojure.lang." "clojure.core."
   "nrepl." "cider."])

(defn- normalize-prefixes
  [exclude]
  (cond
    (nil? exclude) []
    (vector? exclude) exclude
    (sequential? exclude) (vec exclude)
    :else [exclude]))

(defn- resolve-exclusions
  [{:keys [exclude mode] :or {mode :append} :as opts}]
  (let [base default-excluded-prefixes
        normalized (normalize-prefixes exclude)
        has-exclude? (contains? opts :exclude)]
    (case mode
      :replace (if has-exclude? normalized base)
      :append (if (seq normalized)
                (into base normalized)
                base)
      base)))

(defn- count-application-frames-in-stack
  "Count stack frames from application code (skipping Java/Clojure infrastructure).

   Accepts an optional configuration map with:
   - :exclude  vector/seq of namespace or class prefixes to filter out
   - :mode     either :append (default) or :replace

   When :mode is :append, supplied prefixes extend the defaults.
   When :mode is :replace, supplied prefixes fully replace the defaults."
  ([] (count-application-frames-in-stack {}))
  ([maybe-opts]
   (let [opts (cond
                (map? maybe-opts) maybe-opts
                (nil? maybe-opts) {}
                :else {:exclude maybe-opts})
         exclusions (resolve-exclusions opts)
         stack-trace (.getStackTrace (Thread/currentThread))
         app-frames (remove (fn [frame]
                              (some (fn [prefix]
                                      (.startsWith (.getClassName frame) prefix))
                                    exclusions))
                            stack-trace)]
     (count app-frames))))

(defn- convert-to-string
  "Convert various input types to string for Writer.write() method.
   Handles char arrays, strings, and other types."
  ([x off len]
   (cond
     ;; String - extract substring from offset
     (string? x)
     (subs x off (+ off len))

     ;; char[] (Java character array) - convert to String
     (instance? (Class/forName "[C") x)
     (String. ^chars x ^int off ^int len)

     ;; Other types - convert to string representation
     :else
     (str x)))
  ([x]
   (cond
     ;; int/char code - convert to character then string
     (number? x)
     (str (char x))

     ;; Other types - convert to string representation
     :else
     (str x))))

(defn- capture-chunk
  "Capture a text chunk with its current stack depth into the output atom.

  Expects a map with:
  - :output-atom    (required) atom collecting captured chunks.
  - :text           (required) string chunk to capture.
  - :stack-exclude   (optional) map forwarded to `count-application-frames-in-stack`.
                     Recognized keys include :exclude and :mode (:append or :replace).

  Example:
    (capture-chunk {:output-atom chunks
                    :text \"hello\"
                    :stack-exclude {:exclude [\"my.framework.\"] :mode :append}})"
  [{:keys [output-atom text stack-exclude]
    :or {stack-exclude {}}}]
  (let [depth (count-application-frames-in-stack stack-exclude)]
    (swap! output-atom conj {:text text :depth depth})))

(defn- depth-aware-writer
  "Create a Writer that captures both text and stack depth for each write operation.
   Accumulates chunks as {:text string :depth int} maps in the provided atom."
  [output-atom stack-exclude]
  (proxy [java.io.Writer] []
    (write
      ([x off len]
       (capture-chunk {:output-atom output-atom
                       :text (convert-to-string x off len)
                       :stack-exclude stack-exclude}))
      ([x]
       (capture-chunk {:output-atom output-atom
                       :text (convert-to-string x)
                       :stack-exclude stack-exclude})))
    (flush [] nil)
    (close [] nil)))

(defn- process-chunk-with-newlines
  "Process a chunk that contains newlines, splitting it into completed lines
   and returning the new current line state."
  [text depth current-line]
  (let [parts (str/split text #"\n" -1)
        ;; Complete current line with first part
        completed-curr (when-not (str/blank? (str (:text current-line) (first parts)))
                         {:text (str (:text current-line) (first parts))
                          :min-depth (min (:min-depth current-line) depth)})
        ;; Middle parts become complete lines
        middle-lines (keep (fn [part]
                             (when-not (str/blank? part)
                               {:text part :min-depth depth}))
                           (drop 1 (butlast parts)))
        ;; Last part starts new line with fresh min-depth
        new-current {:text (last parts) :min-depth Integer/MAX_VALUE}]
    {:completed (cond-> []
                  completed-curr (conj completed-curr)
                  true (into middle-lines))
     :current new-current}))

(defn- process-chunk-with-newline
  "Process a chunk that contains a newline, splitting it into completed lines."
  [text depth current-line completed-lines]
  (let [{:keys [completed current]} (process-chunk-with-newlines text depth current-line)]
    {:current current
     :completed (into completed-lines completed)}))

(defn- process-chunk-without-newline
  "Process a chunk that has no newline, accumulating it into the current line."
  [text depth current-line completed-lines]
  {:current {:text (str (:text current-line) text)
             :min-depth (min (:min-depth current-line) depth)}
   :completed completed-lines})

(defn- process-single-chunk
  "Process a single chunk, returning updated current line and completed lines.
   Returns a map with :current (new current line state) and :completed (accumulated completed lines)."
  [chunk current-line completed-lines]
  (let [{:keys [text depth]} chunk]
    (if (str/includes? text "\n")
      (process-chunk-with-newline text depth current-line completed-lines)
      (process-chunk-without-newline text depth current-line completed-lines))))

(defn- finalize-current-line
  "Finalize the current line by adding it to completed lines if not blank."
  [current-line completed-lines]
  (if (str/blank? (:text current-line))
    completed-lines
    (conj completed-lines current-line)))

(defn- process-next-chunk
  "Process the next chunk and return updated loop state."
  [remaining current-line completed-lines]
  (let [{:keys [current completed]} (process-single-chunk (first remaining)
                                                          current-line
                                                          completed-lines)]
    {:remaining (rest remaining)
     :current current
     :completed completed}))

(defn- chunks->lines
  "Convert chunks (with text and depth) into lines (with text and min-depth).
   Groups chunks by newlines, tracking the minimum depth seen for each line."
  [chunks]
  (loop [remaining chunks
         current-line {:text "" :min-depth Integer/MAX_VALUE}
         completed-lines []]
    (if (empty? remaining)
      (finalize-current-line current-line completed-lines)
      (let [{:keys [remaining current completed]} (process-next-chunk remaining
                                                                      current-line
                                                                      completed-lines)]
        (recur remaining current completed)))))

(defn- lines->logs
  "Convert lines (with text and min-depth) into log entries with proper indentation.
   Normalizes depths to relative indents and adds base-indent scaled by spacing."
  [lines initial-logs base-indent spacing]
  (if (empty? lines)
    initial-logs
    (let [baseline-depth (apply min (map :min-depth lines))
          indent-step spacing]
      (reduce (fn [logs {:keys [text min-depth]}]
                (let [relative-indent (- min-depth baseline-depth)
                      adjusted-indent (if (odd? relative-indent)
                                        (inc relative-indent)
                                        relative-indent)
                      final-indent (+ base-indent (* indent-step adjusted-indent))]
                  (log/log logs text :indent final-indent)))
              initial-logs
              lines))))

(defn- capture-depth-aware-output-into-logs
  "Capture stdout with depth tracking and merge the result into the log vector.

   Args:
   - initial-logs: The logs vector from before the function call
   - result-logs: The logs vector from after the function call
   - chunks: Vector of {:text string :depth int} maps from depth-aware-writer
   - spacing: Number of spaces to indent per stack depth level

   Returns: Vector of logs with captured output inserted appropriately"
  [initial-logs result-logs chunks spacing]
  (if (empty? chunks)
    result-logs
    (let [lines (chunks->lines chunks)
          last-initial (peek initial-logs)
          base-indent (cond
                        (nil? last-initial) 0
                        (contains? last-initial :indent-next) (:indent-next last-initial)
                        :else (:indent last-initial))
          output-logs (lines->logs lines initial-logs base-indent spacing)
          final-logs (if (identical? result-logs initial-logs)
                       output-logs
                       (concat output-logs (drop (count initial-logs) result-logs)))]
      (vec final-logs))))

(defn- capture-basic-output-into-logs
  "Helper that captures stdout without depth tracking and merges the logs.

   Args mirror `capture-depth-aware-output-into-logs`, but `output` is a StringWriter."
  [initial-logs result-logs output]
  (let [captured (str/split-lines (str output))
        non-empty-lines (remove str/blank? captured)]
    (if (empty? non-empty-lines)
      result-logs
      (let [last-initial (peek initial-logs)
            base-indent (cond
                          (nil? last-initial) 0
                          (contains? last-initial :indent-next) (:indent-next last-initial)
                          :else (:indent last-initial))
            output-logs (reduce (fn [logs line]
                                  (log/log logs line :indent base-indent))
                                initial-logs
                                non-empty-lines)
            final-logs (if (identical? result-logs initial-logs)
                         output-logs
                         (concat output-logs (drop (count initial-logs) result-logs)))]
        (vec final-logs)))))

(defn- run-depth-aware-mode
  "Execute the wrapped function using the depth-aware writer and stack-derived indentation."
  [f input-map initial-logs stack-exclude spacing]
  (let [captured-chunks (atom [])
        writer (depth-aware-writer captured-chunks stack-exclude)
        print-writer (java.io.PrintWriter. writer true)]
    (binding [*out* print-writer]
      (let [result-map (f input-map)
            result-logs (:logs result-map initial-logs)
            final-logs (capture-depth-aware-output-into-logs initial-logs
                                                             result-logs
                                                             @captured-chunks
                                                             spacing)]
        (assoc result-map :logs final-logs)))))

(defn- run-basic-mode
  "Execute the wrapped function using a plain StringWriter (no depth-aware indentation)."
  [f input-map initial-logs]
  (let [writer (java.io.StringWriter.)
        print-writer (java.io.PrintWriter. writer true)]
    (binding [*out* print-writer]
      (let [result-map (f input-map)
            result-logs (:logs result-map initial-logs)
            final-logs (capture-basic-output-into-logs initial-logs
                                                       result-logs
                                                       writer)]
        (assoc result-map :logs final-logs)))))

(defn redirect-stdout
  "Wrap a function so anything printed to `*out*` becomes structured bucket logs.

   Works with both opts-style functions and lifted/bucketized functions since they
   share the same structure (both take/return maps with a `:logs` key).

   Mode-specific behavior:
   - :basic        captures lines verbatim using the caller's base indentation
   - :depth-aware  attempts to measure stack depth so nested calls are indented automatically

   Args:
   - f: function to wrap (takes map with :logs, returns map with :logs)
   - :mode           (optional) either :basic (default) or :depth-aware
   - :spacing        (optional) indentation width per depth level (defaults to half of
                     `default/spacing`, rounded up); only meaningful for :depth-aware
   - :stack-exclude  (optional) map forwarded to `count-application-frames-in-stack`
                     (supports :exclude prefixes and :mode :append/:replace); only used for
                     :depth-aware mode

   Returns: wrapped function that captures output into logs.

   Example - opts-style function:
     (defn legacy-fn [opts]
       (println \"Starting process...\")
       (pr {:status \"processing\"})
       (print \"Done.\")
       (bucket/grab 42 :logs (:logs opts [])))

     (def wrapped (redirect-stdout legacy-fn))
     (wrapped {:logs []})
     ;; => Bucket with :value 42 and :logs containing captured output

   Example - lifted function:
     (defn plain-fn [x]
       (println \"Processing\" x)
       (* x 2))

     (def lifted (bucket/bucketize plain-fn))
     (def captured (redirect-stdout lifted
                                    :mode :depth-aware
                                    :stack-exclude {:exclude [\"fb.actions.\"] :mode :append}))
     (captured (bucket/grab 5 :logs []))
     ;; => Bucket with :value 10 and captured output in logs"
  [f & {:keys [mode spacing stack-exclude]
        :or {mode :basic
             spacing (int (math/ceil (/ default/spacing 2.0)))
             stack-exclude {}}}]
  (let [wrapped (fn [input-map]
                  (let [initial-logs (:logs input-map [])]
                    (case mode
                      :depth-aware
                      (run-depth-aware-mode f input-map initial-logs stack-exclude spacing)

                      :basic
                      (run-basic-mode f input-map initial-logs)

                      (throw (ex-info "Unsupported redirect-stdout mode." {:mode mode})))))]
    (with-meta wrapped (meta f))))
