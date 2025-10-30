(ns bucket.log.secret
  (:require [clojure.string :as str]))

(def sensitive-patterns
  {:exact-matches #{"confidential" "encrypted" "hash" "pin"}
   :contains ["api" "auth" "cert" "cipher" "cred" "key" "pass" "pw" "secret" "sha" "signature" "token"]
   :word-boundaries ["pin" "sec" "sig"]
   :compound-patterns ["api.*key" "api.*val" "key.*val"]
   :url-with-credentials #"(http|https):\/\/.*:(.*)@"})

(defn likely-secret?
  "Check if a string likely contains sensitive information."
  [input]
  (let [lower-input (str/lower-case input)]
    (or
     (contains? (:exact-matches sensitive-patterns) lower-input)
     (some #(str/includes? lower-input %)
           (:contains sensitive-patterns))
     (boolean (some #(re-find (re-pattern (str "(?<![a-zA-Z0-9])" % "(?![a-zA-Z0-9])")) lower-input)
                    (:word-boundaries sensitive-patterns)))
     (some #(re-find (re-pattern (str "(?i)" %)) input)
           (:compound-patterns sensitive-patterns))
     (boolean (re-find (:url-with-credentials sensitive-patterns) input)))))
