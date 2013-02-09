import Utils

-- sum all primes with a value < x
-- use 2000000 for problem #10

primeSum :: Double -> Double
primeSum x = sum $ takeWhile ((>) x) $ filter prime [2..]