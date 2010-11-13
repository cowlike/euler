(println (apply + (filter #(zero? (* (mod % 3)(mod % 5))) (range 1000))))
