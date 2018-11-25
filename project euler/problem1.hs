module Problem1 where

-- Could call sum on filter where the filter
-- would only return true if the number was a multiple
-- of 3 or 5

-- g :: [Integer] -> Integer  
g :: [Integer] -> Integer
g = sum $ filter (\y -> y `mod` 3 == 0 || y `mod` 5 == 0)

-- Using list comprehension
sum [x | x <- [1..9999], x `mod` 3 == 0 || x `mod` 5 == 0]

