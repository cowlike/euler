(defn mkodd [n]
  (if (even? n) (inc n) n))

(defn prime [n]
  (cond
    (< n 2) false
    (= n 2) true
    (= n 3) true
    (= 0 (mod n 2)) false
    (= 0 (mod n 3)) false
    (empty? (filter #(= 0 (mod n %)) 
      (range (mkodd (int (Math/floor (Math/sqrt n)))) 3 -2))) true
    :else false))

(defn foo [num]
  (loop [n 3 count 2]
    (if (= num count) n
      (recur (+ n 2) (if (prime (+ n 2)) (inc count) count)))))

(defn foo2 [num]
  (first (drop (dec num) (filter prime (drop 2 (range))))))

(defn foo3 [num]
  (->> (range)
    (drop 2)
    (filter prime)
    (drop (dec num))
    (first)))
