divideThenAdd :: Num a => a -> a -> a
divideThenAdd x y = (x / y) + 1


map' :: a -> b -> [a] -> [b]
map' f []       = []
map' f (x : xs) = f x : map' f xs
