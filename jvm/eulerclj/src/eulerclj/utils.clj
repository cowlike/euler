(ns eulerclj.utils
  (:require [clojure.math.numeric-tower :refer [sqrt]])
  (:require [clojure.string :as cs]))

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
  "get nth term of fibonacci sequence (horribly inefficient)"
  [n] 
  (condp = n
    0 0
    1 1
    (+ (fib (dec n)) (fib (- n 2)))))

(defn fibfast
  "returns the fibonacci sequence where each term is less than a max value"
  ([] (fibfast 100 '(1 0)))
  ([max] (fibfast max '(1 0)))
  ([max coll] (if (> (first coll) max) 
                (reverse (rest coll)) 
                (recur max (cons (+ (first coll) (second coll)) coll )))))

(defn rcount
  "tail recursive count of elements"
  [xs]
  (loop [acc 0 xs xs]
    (cond
      (empty? xs) acc
      :else (recur (inc acc) (rest xs)))))

(defn palindromic 
  "test if arg is palindromic. works on collections and numbers"
  [s]
  (let [xs (if (number? s) (seq (str s)) (seq s))]
    (= xs (reverse xs))))
