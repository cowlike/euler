(def difflist (partition 2 1 (map #(* % %) (range 1 1000))))

(map #(apply - %) difflist)

(apply str (map (fn [[a b]] (- b a)) [[1 5] [6 8]]))
