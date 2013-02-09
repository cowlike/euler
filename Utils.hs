module Utils (
prime
,fib
) where

prime :: Integral a => a -> Bool

prime x
      | x < 2 = False
	  | x == 2 || x == 3 = True
	  | x `mod` 2 == 0 || x `mod` 3 == 0 = False

prime x = foldl (\acc n -> acc && (not $ x `mod` n == 0)) True [5,7..o]
	      where   
		     f = floor $ sqrt $ fromIntegral x
		     o = if even f then f + 1 else f
		
fib :: Int -> Int

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)