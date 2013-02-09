module Utils where

prime :: Double -> Bool

prime x
      | x < 2 = False
	  | x == 2 || x == 3 = True
	  | i `mod` 2 == 0 || i `mod` 3 == 0 = False
	  where i = floor x

prime x = foldl (\acc n -> acc && (not $ (floor x) `mod` n == 0)) True [5,7..o]
	      where   
		     f = floor . sqrt $ x
		     o = if even f then f + 1 else f