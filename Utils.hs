module Utils (
isPrime
,isPrime'
,fib
) where

isPrime :: Integral a => a -> Bool
isPrime x
      | x < 2 = False
	  | x == 2 || x == 3 = True
	  | x `mod` 2 == 0 || x `mod` 3 == 0 = False

isPrime x = foldl (\acc n -> (not $ x `mod` n == 0) && acc) True [5,7..o]
	      where   
		     f = floor $ sqrt $ fromIntegral x
		     o = if even f then f + 1 else f
		
-- **************************************************************
isPrime' :: Integral a => a -> Bool
isPrime' x
      | x < 2 = False
	  | x == 2 || x == 3 = True
	  | x `mod` 2 == 0 || x `mod` 3 == 0 = False

isPrime' x = foldr (\n acc -> (not $ x `mod` n == 0) && acc) True [5,7..o]
	      where   
		     f = floor $ sqrt $ fromIntegral x
		     o = if even f then f + 1 else f
			 
-- **************************************************************
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
