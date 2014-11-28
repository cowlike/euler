(ns eulerclj.utils
  (:require [clojure.math.numeric-tower :refer [sqrt]]))

(defn prime? [n]
  (letfn [(p-test [num first]
            (loop [n num i first]
              (cond
                (> (* i i) n) true
                (= 0 (mod n i)) false
                (= 0 (mod n (+ i 2))) false
                :else (recur n (+ i 6)))))]
    (cond
      (<= n 3) (> n 1)
      (= 0 (mod n 2)) false
      (= 0 (mod n 3)) false
      :else (p-test n 5))))

(defn gen-primes
  "Brute force... should use sieve but whatever"
  []
  (filter prime? (drop 2 (range))))

(defn prime-divisors
  "return a sequence of divisors of n that are prime"
  [n]
  (filter #(and (prime? %) (zero? (mod n %))) (range 2 (sqrt n))))

(defn fib
  "get nth term of fibonacci sequence"
  [n] 
  (condp = n
    0 0
    1 1
    (+ (fib (dec n)) (fib (- n 2)))))

(defn fibfast
  ([] (fibfast 100 [0 1]))
  ([max] (fibfast max [0 1]))
  ([max coll] (if (> (last coll) max) (drop-last coll) (recur max (conj coll (+ (last coll) (last (butlast coll))))))))
