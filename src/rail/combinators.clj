(ns rail.combinators
    (:require [rail.core :as r]
              [schema.core :as s])
    (:refer-clojure :exclude [vector])
    )

(defn- all-success [results]
  (or (empty? results)
      (->> results
           (map r/get-branch)
           set
           (= #{:success}))))

(s/defn vector :- r/Result
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
