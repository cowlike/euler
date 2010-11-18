(defn combo-mult [n]
  (let [rmult (fn [n] (map #(* n %) (range n 0 -1)))]
    (flatten (map rmult (range n 0 -1)))))

(defn palindromic [n]
  (let [s (str n)]
    (= s (apply str (reverse s)))))

(max (sort > (filter palindromic (combo-mult 999))))