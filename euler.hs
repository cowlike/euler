import Utils
import qualified Data.List as L

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

e11data = [
    08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08
    ,49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00
    ,81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65
    ,52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91
    ,22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80
    ,24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50
    ,32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70
    ,67,26,20,68,02,62,12,20,95,63,94,39,63,08,40,91,66,49,94,21
    ,24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72
    ,21,36,23,09,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95
    ,78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14,09,53,56,92
    ,16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57
    ,86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58
    ,19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40
    ,04,52,08,83,97,35,99,16,07,97,57,32,16,26,26,79,33,27,98,66
    ,88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69
    ,04,42,16,73,38,25,39,11,24,94,72,18,08,46,29,32,40,62,76,36
    ,20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16
    ,20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54
    ,01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48]

-- e11 helper. assign coordinates to each position in a list. 2nd arg is row len
makeCoord :: [a] -> Int -> [(Int, Int, a)]
makeCoord s r = [(x, y, s !! (y * r + x)) | y <- [0..rows - 1], x <- [0..r - 1]]
                where rows = floor $ (fromIntegral $ length s) / (fromIntegral r)

get :: Eq a => [(Int, Int, a)] -> Int -> Int -> Maybe (Int, Int, a)
get lst x y = let result = filter (\(tx,ty,_) -> tx == x && ty == y) lst in
    if (result == [])
        then Nothing 
        else Just (head result)

val (_,_,x) = x
        
zget :: [(Int, Int, Int)] -> (Int, Int) -> Int
zget lst (x, y) = let result = filter (\(tx,ty,_) -> tx == x && ty == y) lst in
    if (result == [])
        then 0
        else val $ head result

myget = zget $ makeCoord e11data 20

localNums :: Int -> Int -> [[(Int,Int)]]
localNums x y = 
    let z1 = zip [x..x+3] (repeat y)
        z2 = zip [x..x+3] [y..y+3]              
        z3 = zip (repeat x) [y..y+3]
        z4 = zip [x,x-1..x-3] [y..y+3] in
    z1 : z2 : z3 : z4 : []

--e11 solution
--foldl1 max $ foldl1 (++) [map product $ map (map myget) (localNums x y) | y <- [0..19], x <- [0..19]]

trinum :: Integer -> Integer
trinum x = L.foldl1' (+) [1..x]

divisors :: Integer -> [Integer]
divisors x = x : [div | div <- [1..floor $ fromInteger x / 2], x `mod` div == 0]
--head [n | n <- [trinum x | x <- [1000..]], len' $ divisors n > 500]

