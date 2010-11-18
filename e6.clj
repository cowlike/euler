(defn euler6 [n]
  (let [nums (range 1 (inc n))
        sum (reduce + nums)
        sum-sq (reduce + (map #(* % %) nums))]
    (- (* sum sum) sum-sq)))