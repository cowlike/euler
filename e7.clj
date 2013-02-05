(defn mkodd [n]
  (if (even? n) (inc n) n))

(defn prime [n]
  (cond
    (< n 4) true
    (= 0 (mod n 2)) false
    (empty? (filter #(= 0 (mod n %)) 
      (range (mkodd (int (Math/floor (/ n 2)))) 1 -2))) true
    :else false))

(defn foo [num]
  (loop [n 3 count 2]
    (if (= num count) n
      (recur (+ n 2) (if (prime (+ n 2)) (inc count) count)))))