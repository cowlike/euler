import Utils

e1 = sum $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [1..999]

e2 = sum $ takeWhile ((>) 4000000) [x | x <- [fib x | x <- [1..]], even x]

-- n = 600851475143 for problem #3
e3 :: (Integral a) => a -> a
e3 n = head [x | x <- [f, f-1 .. 3], n `mod` x == 0, isPrime x]
       where f = floor $ sqrt $ fromIntegral n

-- problem #4 uses 2 3-digit numbers
-- foldl1 max $ e4 [100..999] [100..999]
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

e6 :: [Int] -> Int
e6 xs = sum xs * sum xs - sum [x * x | x <- xs]

-- n = 10001 for problem #7
e7 :: Int -> Int
e7 n = head . drop (n - 1) . filter isPrime' $ [2..]

-- data for problem #8
e8data = foldl1 (++) [
     "73167176531330624919225119674426574742355349194934"
     ,"96983520312774506326239578318016984801869478851843"
     ,"85861560789112949495459501737958331952853208805511"
     ,"12540698747158523863050715693290963295227443043557"
     ,"66896648950445244523161731856403098711121722383113"
     ,"62229893423380308135336276614282806444486645238749"
     ,"30358907296290491560440772390713810515859307960866"
     ,"70172427121883998797908792274921901699720888093776"
     ,"65727333001053367881220235421809751254540594752243"
     ,"52584907711670556013604839586446706324415722155397"
     ,"53697817977846174064955149290862569321978468622482"
     ,"83972241375657056057490261407972968652414535100474"
     ,"82166370484403199890008895243450658541227588666881"
     ,"16427171479924442928230863465674813919123162824586"
     ,"17866458359124566529476545682848912883142607690042"
     ,"24219022671055626321111109370544217506941658960408"
     ,"07198403850962455444362981230987879927244284909188"
     ,"84580156166097919133875499200524063689912560717606"
     ,"05886116467109405077541002256983155200055935729725"
     ,"71636269561882670428252483600823257530420752963450"]
	 
e8 :: [Char] -> Int
e8 s | length s < 5 = 0
e8 s = foldl1 max $ map (\s -> product $ foldl (\acc n -> (read [n] :: Int) : acc) [] s) (partition s 5 1)

e9 = head $ let r = [1..999] in 
            [a * b * c | a <- r, b <- r, c <- r, 
			  a < b && b < c, 
		      a + b + c == 1000, 
			  a*a + b*b == c*c]

-- sum all primes with a value < x
-- use 2000000 for problem #10
e10 :: Integer -> Integer
e10 x = sum $ takeWhile ((>) x) $ filter isPrime [2..]

{-
e11
let s = "abcdefghi" in [(x `mod` 3, floor $ (fromIntegral x) / 3, s !! x) | x <- [0..length s - 1]]
-}
-- e11 helper. assign coordinates to each position in a list
makeCoord :: [a] -> [(Int, Int, a)]
makeCoord s = [(x `mod` 3, floor $ (fromIntegral x) / 3, s !! x) | x <- [0..length s - 1]]
