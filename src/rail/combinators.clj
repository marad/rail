(ns rail.combinators
    (:require [rail.core :as r]
              [schema.core :as s]))

(defn- all-success [results]
  (or (empty? results)
      (->> results
           (map r/get-branch)
           set
           (= #{:rail/success}))))

(s/defn all :- r/Result
  "Given collection of Result returns single Result with values combined into a vector.
  Merges all messages together. If even one result is a failure merges messages and returns failure"
  [results :- [r/Result]]
  (let [messages (->> results
                      (map :messages)
                      (apply concat))]
    (if (all-success results)
      (r/succeed (->> results
                      (map :value)
                      (apply clojure.core/vector)) messages)
      (r/fail messages))))

(s/defn any :- r/Result
  "Given a collection of Result returns first success result or default.
  If default is not provided returns (r/fail []). If there is no success
  returns last error"
  ([results :- [r/Result]]
   (any (r/fail []) results))

  ([default :- r/Result, results :- [r/Result]]
   (reduce
     (fn [acc x] (if (r/success? acc) acc x))
     default
     results)))
