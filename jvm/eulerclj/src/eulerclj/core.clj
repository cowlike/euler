(ns eulerclj.core
  (:use eulerclj.utils)
  (:gen-class))

(defn e1 []
  (->> 1000
    range
    (filter #(zero? (* (mod % 3)(mod % 5))))
    (apply +)))
    
(defn e2 []
  (apply + (filter even? (fibfast 4000000))))

(defn e3 []
  (-> 600851475143 prime-divisors last))
  
(defn e4 []
  (apply max 
         (let [xs (range 999 99 -1)]
           (for [x xs y xs
                 :let [z (* x y)]
                 :when (palindromic z)]
             z))))

(defn e5 []
  (let [r (range 11 21)] 
    (loop [n 40] 
      (if (every? zero? (map #(mod n %) r)) 
        n 
        (recur (+ 2 n))))))

(defn e6 []
  (let [nums (range 1 101)
        sum (reduce + nums)
        sum-sq (reduce + (map #(* % %) nums))]
    (- (* sum sum) sum-sq)))

(defn e7 []
  (last (take 10001 (gen-primes))))

(defn e8 []
  (let [part-size 13
        s "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
        mknums (fn [xs] (map #(- (int %) (int \0)) xs))]
    (->> 
      (partition part-size 1 s)
      (map mknums)
      (map #(reduce * %))
      (apply max))))

(defn e9 []
  (let [n 998]
    (for [a (range 1 n) b (range (inc a) n) c (range (inc b) n)
          :when (= (+ (* a a) (* b b)) (* c c))
          :when (= 1000 (+ a b c))]
      (* a b c))))

(defn e10 []
  (apply + (for [x (gen-primes) :when (< x 2000000)] x)))

(declare e15m)

(defn e15 [x y]
  (cond
    (every? zero? [x y]) 1
    (zero? x) (e15m x (dec y))
    (zero? y) (e15m (dec x) y)
    :else (+ (e15m (dec x) y) (e15m x (dec y)))))

(def e15m (memoize e15))

(defn pow [n exp] 
  (loop [n n exp exp result (bigint 1)] 
    (if (zero? exp) 
      result 
      (recur n (dec exp) (* result n)))))

(def e16 (apply + 
                (map #(- (int %) 48) 
                     (str (pow 2 1000)))))

;;========================================
(defn -main
  "run some Euler functions"
  [& args]
  (println "e3 = " (e3)))

