(defn fib
  ([] (fib 100 [0 1]))
  ([max] (fib max [0 1]))
  ([max coll] (if (> (last coll) max) (drop-last coll) (recur max (conj coll (+ (last coll) (last (butlast coll))))))))

(println (apply + (filter even? (fib 4000000))))
