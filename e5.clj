(let [divs (range 2 21)]
  (loop [n 40]
    (if (every? zero? (map #(mod n %) divs)) n (recur (+ 2 n)))))