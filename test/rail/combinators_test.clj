(ns rail.combinators-test
    (:require [clojure.test :refer :all]
              [rail.combinators :refer :all]
              [rail.core :as r]))

(deftest test-all-combinator
  (is (= (all []) (r/succeed [])))
  (is (= (all [(r/succeed 42 [:foo]) (r/succeed 24 [:bar])])
         (r/succeed [42 24] [:foo :bar])))
  (is (= (all [(r/succeed 42 [:foo]) (r/fail [:bar])])
         (r/fail [:foo :bar]))))

(deftest test-any-combinator
  (is (= (any (r/fail [:foo]) []) (r/fail [:foo])))
  (is (= (any (r/fail [:foo]) [(r/succeed 42)]) (r/succeed 42)))
  (is (= (any (r/fail [:foo]) [(r/fail [:bar])]) (r/fail [:bar])))
  (is (= (any (r/fail [:foo]) [(r/fail [:bar]) (r/succeed 42)])
         (r/succeed 42))))
