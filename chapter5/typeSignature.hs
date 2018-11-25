module TypeSignature where


--functionH :: [a] -> a
functionH (x:_) = x

--functionC :: Ord a => a -> a -> Bool
functionC x y =
    if (x > y) then True else False

--functionS :: (a, b) -> b
functionS (x, y) = y


i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r (x:xs) = xs

co :: (b -> c) -> (a -> b) -> a -> c
co f g x = f $ g x

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f x = f x

