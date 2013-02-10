import Utils

e1 = sum $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [1..999]

e2 = sum $ takeWhile ((>) 4000000) [x | x <- [fib x | x <- [1..]], even x]

-- n = 600851475143 for problem #3
e3 :: (Integral a) => a -> a
e3 n = head [x | x <- [f, f-1 .. 3], n `mod` x == 0, isPrime x]
       where f = floor $ sqrt $ fromIntegral n

-- problem #4 uses 2 3-digit numbers
-- foldl max 0 $ e4 [100..999] [100..999]
e4 :: [Int] -> [Int] -> [Int]
e4 xs ys = [x * y | x <- xs, y <- ys, let s = show(x * y) in s == reverse s]

-- a helper for e5, x is evenly divisible by every number in the range
divBy :: Int -> [Int] -> Bool
divBy x range = foldl (\acc v -> x `mod` v == 0 && acc) True range

e5 :: Int -> Int -> Int
e5 from to
    | from < 0 = 0
	| to <= from = 0
e5 from to = head $ dropWhile (\n -> not $ divBy n [from .. to-1]) [to, to + to ..]

