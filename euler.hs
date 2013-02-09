import Utils

e1 = sum $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [1..999]

e2 = sum $ takeWhile ((>) 4000000) [x | x <- [fib x | x <- [1..]], even x]

-- n = 600851475143 for problem #3
e3 :: (Integral a) => a -> a
e3 n = head [x | x <- [f, f-1 .. 3], n `mod` x == 0, isPrime x]
       where f = floor $ sqrt $ fromIntegral n
