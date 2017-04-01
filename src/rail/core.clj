(ns rail.core
    (:require [schema.core :as s]
              [clojure.test :refer [function?]])
    (:refer-clojure :exclude [map apply]))


(declare get-branch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema

(s/defschema Value s/Any)
(s/defschema Message s/Any)
(s/defschema Success {:branch (s/eq :success)
                      :value Value
                      :messages [Message]})
(s/defschema Failure {:branch (s/eq :failure)
                      :value Value
                      :messages [Message]})
(s/defschema Result (s/if #(= (get-branch %) :success) Success Failure))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Parts

(s/defn make-branch [branch value messages]
  {:branch branch, :value value, :messages (clojure.core/apply vector messages)})

(s/defn get-branch [result] (:branch result))

(s/defn succeed :- Result
  "Create a success result. Optionally with messages."
  ([value :- Value] (make-branch :success value []))
  ([value :- Value, messages :- [Message]] (make-branch :success value messages)))

(def ok succeed)

(s/defn fail :- Result
  "Create fail result."
  [message]
  (if (coll? message)
    (make-branch :failure nil message)
    (make-branch :failure nil [message])))

(s/defn either :- Result
  "Applies f-success if on success branch or fFailure if on failure branch.
  f-failure takes Value and list of Messages and should return Result.
  fFailure takes list of Messages and returns Result"
  [f-success f-failure {:keys [branch value messages]} :- Result]
  (case branch
    :success (f-success value messages)
    :failure (f-failure messages)))

(s/defn merge-messages :- Result
  "Merges additional messages to result"
  [msgs :- [Message]
   result :- Result]
  (update result :messages #(concat msgs %)))

(s/defn bind :- Result
  "Given function f :- Value -> Result applies it on success branch"
  [f result :- Result]
  (either #(->> %1 f (merge-messages %2))
          fail
          result))

;; Add an alias
(def flat-map bind)

;; TODO: add ability to apply multiple results
(s/defn apply :- Result
  "Given a function as a result applies the function on the
  result if both are on :success branch"
  [{f :value f-branch :branch f-msgs :messages} :- Result
   {r :value r-branch :branch r-msgs :messages} :- Result]
  (case [f-branch r-branch]
     [:success :success] (succeed (f r) (concat f-msgs r-msgs))
      (fail (concat f-msgs r-msgs))))

(s/defn lift :- Result
  "Given a function f :- Value -> Value applies it if on :success branch"
  [f result :- Result]
  (apply (succeed f) result))

;; Add an alias
(def map lift)

(s/defn success-side-effect :- Result
  "Given a function f :- Value, [Message] -> Result and a Result applies the function
  to the success branch and returns result unchanged"
  [f {b :branch v :value msgs :messages :as result} :- Result]
  (when (= b :success)
    (f v msgs))
  result)

(def success-tee success-side-effect)

(s/defn failure-side-effect :- Result
  "Given a function f :- [Message] -> Result and a Result applies the function
  to the failure branch and returns result unchanged"
  [f {b :branch msgs :messages :as result} :- Result]
  (when (= b :failure)
    (f msgs))
  result)

(def failure-tee failure-side-effect)

(s/defn map-success :- Result
  "Given a function f :- Value, [Message] -> Result transforms success branch
  and returns a result"
  [f result :- Result]
  (either f fail result))

(s/defn map-messages :- Result
  "Given a function f :- Message -> Message applies function to result messages"
  [f result :- Result]
  (update result :messages #(clojure.core/map f %)))

(s/defn get-or-default :- Value
  "Returns the value from Result or the default. Default can be function or value."
  [default {branch :branch v :value msgs :messages} :- Result]
  (case branch
    :success v
    :failure (if (function? default)
               (try (default)
                    (catch clojure.lang.ArityException ex
                           (default msgs)))
               default)))

(s/defn fail-if-nil
  "Creates a Result from a value. If value is nil fails with given message"
  ([message]
   (fn [value messages]
       (if-not (nil? value)
               (succeed value messages)
               (fail (conj messages message)))))
  ([message value]
   (if-not (nil? value)
           (succeed value)
           (fail message))))

(s/defn success? :- s/Bool
  [result :- Result]
  (= (get-branch result) :success))

(s/defn fail? :- s/Bool
  [result :- Result]
  (= (get-branch result) :failure))
