module Lab03 where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens [] = []
evens (x:xs)
  | even x = x : evens xs
  | otherwise = evens xs

-- ===================================
-- Ex. 3 - 4 
-- ===================================

-- complete the following line with the correct type signature for this function
-- squares :: ... 
squares :: Integer -> [Integer]
squares 0 = []
squares n = squares (n-1) ++  [n*n]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
-- squares' :: ...
squares' :: Integer -> Integer -> [Integer]
squares' m n = drop (fromInteger n) $ squares (m+n)

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)

-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords n m = [(x, y) | x <- xs, y <- ys]
  where
    xs = [0..n]
    ys = [0..m]
