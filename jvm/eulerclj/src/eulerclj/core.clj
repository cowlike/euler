(ns eulerclj.core
  (use eulerclj.utils)
  (:gen-class))

(defn -main
  "run some Euler functions"
  [& args]
  (println "e3 = " (e3)))

(defn e1 []
  (->> 1000
    range
    (filter #(zero? (* (mod % 3)(mod % 5))))
    (apply +)))
    
(defn e2 []
  (apply + (filter even? (fibfast 4000000))))

(defn e3 []
  (-> 600851475143 prime-divisors last))
  
