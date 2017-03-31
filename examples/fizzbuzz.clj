(ns examples.fizz-buzz
    (:require [rail.core :as r]
              [clojure.string :as str]
              ))

;; Example fizz buzz solution with rail
;; This example is partly based on blog post by Scott Wlaschin
;; https://fsharpforfunandprofit.com/posts/railway-oriented-programming-carbonated/

(defn rule [divisor label]
  (fn [i]
      (if (= 0 (mod i divisor))
        (r/succeed i [label])
        (r/succeed i))))

(defn show [i msgs]
  (if (empty? msgs)
    (r/succeed (str i))
    (r/succeed (str/join msgs))))

(defn fizz-buzz [i]
  (->> (r/succeed i)
       (r/bind (rule 3 "Fizz"))
       (r/bind (rule 5 "Buzz"))
       (r/map-success show)
       (r/get-or-default "")))

(comment
  (->> (iterate inc 1)
       (map fizz-buzz)
       (take 16))
  )


