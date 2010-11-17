(defn mkodd [n]
  (if (even? n) (inc n) n))

(defn prime [n]
  (cond
    (< n 4) true
    (= 0 (mod n 2)) false
    (empty? (filter #(= 0 (mod n %)) 
      (range (mkodd (int (Math/floor (/ n 2)))) 3 -2))) true
    :else false))

(defn prime-divisors [num]
  (let [sq-n (mkodd (int (Math/floor (Math/sqrt num))))]
    (filter #(> % 0) (map #(if (and (= 0 (mod num %)) (prime %)) % -1) (range sq-n 2 -2)))))

(println (prime-divisors 600851475143))