module Utils where

--prime :: Double -> Bool
prime :: Int -> Bool

prime x
      | x < 2 = False
	  | x == 2 || x == 3 = True
	  | x `mod` 2 == 0 || x `mod` 3 == 0 = False

prime x = foldl (\acc n -> acc && (not $ x `mod` n == 0)) True [5,7..o]
	      where   
		     f = floor $ sqrt $ fromIntegral x
		     o = if even f then f + 1 else f