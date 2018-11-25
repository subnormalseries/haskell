module Chapter4Exercises where

-- length :: [a] -> Integer
-- length [1, 2, 3, 4, 5] = 5
-- length [(1,2), (2,3), (3,4)] = 3
-- length awesome = 2
-- length allAwesome = 2

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else negate x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = (,) ((,) (snd x) (snd y)) $ (,) (fst x) (fst y)

lengthPlusOne :: String -> Int
lengthPlusOne xs = x w 1
 where
  w = length xs
  x = (+)

id' :: a -> a
id' x = x

-- f' :: Num a => (a,a) -> Int
f' x = if (fst x == 1) && (snd x == 2) then 1 else 0


-- This isn't the same as the fst and snd defined in the Prelude for tuples.
-- This function just takes 2 arguments and returns either the first or the second.
fst' :: a -> b -> a
fst' x _ = x

snd' :: a -> b -> b
snd' _ y = y

fst'' :: (a, b) -> a
fst'' (x, _) = x

snd'' :: (a, b) -> b
snd'' (_, b) = b

swap' :: (a, b) -> (b, a)
swap' (a, b) = (b, a)
