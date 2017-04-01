(ns rail.combinators-test
    (:require [clojure.test :refer :all]
              [rail.combinators :refer :all]
              [rail.core :as r])
    (:refer-clojure :exclude [vector])
    )

(deftest test-vector-combinator
  (is (= (vector []) (r/succeed [])))
  (is (= (vector [(r/succeed 42 [:foo]) (r/succeed 24 [:bar])])
         (r/succeed [42 24] [:foo :bar])))
  (is (= (vector [(r/succeed 42 [:foo]) (r/fail [:bar])])
         (r/fail [:foo :bar]))))
