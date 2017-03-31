(ns rail.core-test
    (:require [clojure.test :refer :all]
              [schema.core :as s]
              [rail.core :refer :all])
    (:refer-clojure :exclude [map apply]))

(s/set-fn-validation! true)

(deftest test-simple-stuff
  (testing "succeed and fail"
           (is (= (make-branch :success 1 [2 3]) {:branch :success, :value 1, :messages [2 3]}))
           (is (= (succeed 1) (make-branch :success 1 [])) )
           (is (= (succeed 1 [2 3]) (make-branch :success 1 [2 3])))
           (is (= (fail :msg) (make-branch :failure nil [:msg])))))

(deftest test-either
  (testing "applying to success branch"
           (is (= (either (fn [v msgs] (succeed (inc v) (conj msgs :test)))
                          fail
                          (succeed 42 [:msg]))
                  (succeed 43 [:msg :test])
                  )))
  (testing "applying to fail branch"
           (is (= (either succeed
                          (fn [msgs] (fail (conj msgs :test)))
                          (fail :msg)
                          )
                  (fail [:msg :test])
                  ))))

(deftest test-merging-messages
  (testing "merging success messages"
           (is (= (merge-messages [:test] (succeed 42 [:msg]))
                  (succeed 42 [:test :msg]))))
  (testing "merging fail messages"
           (is (= (merge-messages [:test] (fail :msg))
                  (fail [:test :msg])
                  ))))

(deftest test-binding
  (testing "binding works"
           (is (= (bind #(succeed (inc %) [:foo]) (succeed 42 [:bar]))
                  (succeed 43 [:bar :foo])))
           (is (= (bind (fn [_] (fail [:foo])) (succeed 42 [:bar]))
                  (fail [:bar :foo])))
           (is (= (bind (fn [_] (fail [:foo])) (fail [:bar]))
                  (fail [:bar])))))

(deftest test-applying
  (testing "applying works"
           (is (= (apply (succeed inc [:foo]) (succeed 42 [:bar]))
                  (succeed 43 [:foo :bar])))
           (is (= (apply (succeed inc [:foo]) (fail [:bar]))
                  (fail [:foo :bar])))
           (is (= (apply (fail [:foo]) (succeed 42 [:bar]))
                  (fail [:foo :bar])))
           (is (= (apply (fail [:foo]) (fail [:bar]))
                  (fail [:foo :bar])))))

(deftest test-map
  (testing "mapping works"
           (is (= (map inc (succeed 42 [:foo]))
                  (succeed 43 [:foo])))
           (is (= (map inc (fail :bar))
                  (fail :bar)))))

(deftest test-side-effects
  (testing "sucess side effect"
           (let [env (atom nil)]
             (is (= (success-side-effect #(reset! env [%1 %2]) (succeed 42 [:foo]))
                    (succeed 42 [:foo])))
             (is (= @env [42 [:foo]])))
           (let [env (atom nil)]
             (is (= (success-side-effect #(reset! env [%1 %2]) (fail [:foo]))
                    (fail [:foo])))
             (is (= @env nil))))
  (testing "failure side effect"
           (let [env (atom nil)]
             (is (= (failure-side-effect #(reset! env %) (succeed 42 [:foo]))
                    (succeed 42 [:foo])))
             (is (= @env nil)))
           (let [env (atom nil)]
             (is (= (failure-side-effect #(reset! env %) (fail [:foo]))
                    (fail [:foo])))
             (is (= @env [:foo])))))

(deftest test-mapping-success
  (testing "mapping success"
           (is (= (map-success #(succeed (inc %1) (conj %2 :bar)) (succeed 42 [:foo]))
                  (succeed 43 [:foo :bar])))
           (is (= (map-success #(succeed (inc %1) (conj %2 :bar)) (fail :foo))
                  (fail :foo)))))

(deftest test-mapping-messages
  (testing "mapping messages"
           (is (= (map-messages inc (succeed 42 [1 2 3 4]))
                  (succeed 42 [2 3 4 5])
                  ))
           (is (= (map-messages inc (fail [1 2 3 4]))
                  (fail [2 3 4 5])))))

(deftest test-getting-value
  (testing "getting value or default"
           (is (= (get-or-default nil (succeed 42)) 42))
           (is (= (get-or-default nil (fail :foo)) nil))
           (is (= (get-or-default 42 (fail :foo)) 42))
           (is (= (get-or-default (fn [] 42) (fail :foo)) 42))
           (is (= (get-or-default (fn [[msg]] msg) (fail :foo)) :foo))))

(deftest test-fail-if-nil
  (testing "fail if nil"
           (is (= (fail-if-nil :foo 42) (succeed 42)))
           (is (= (fail-if-nil :foo nil) (fail :foo)))))
