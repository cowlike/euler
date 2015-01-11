module Utils (
isPrime
,genPrimes
,fib
,partition
) where

isPrime :: Integral a => a -> Bool
isPrime n
  | n <= 3 = n > 1
  | n `mod` 2 == 0 || n `mod` 3 == 0 = False
  | otherwise = testPrime n 5
      where testPrime n i
              | i * i > n = True
              | n `mod` i == 0 || n `mod` (i + 2) == 0 = False
              | otherwise = testPrime n (i + 6)
             
genPrimes = [x | x <- [2..], isPrime x]

-- **************************************************************
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

partition :: [a] -> Int -> Int -> [[a]]
partition str partBy step 
          | length str < partBy = []
          | otherwise = take partBy str : partition (drop step str) partBy step

pairs xs = zip xs (tail xs)