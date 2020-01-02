{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where
    example = 1

    a :: Num a => a 
    a = (*9) 6

    -- Num a => (a, [Char])
    b = head [(0, "doge"), (1, "kitteh")]

    -- (Integer, [Char])
    c = head [(0 :: Integer, "doge"),(1, "kitteh")]

    -- Bool
    d = if False then True else False

    -- Int
    e = length [1, 2, 3, 4, 5]

    -- Bool
    f = (length [1, 2, 3, 4]) > (length "TACOCAT")

    h :: (Num a, Num b) => a -> b -> b; h = undefined

    kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined


    x = 5
    y = x + 5
    f' = 4 / y


    bigNum = (^) 5 $ 10


    myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
    myFunc f g _ (s, t) = (,) s $ g(f t)

    
    co :: (b -> c) -> (a -> b) -> (a -> c)
    co f g = f . g


    head' :: [Char] -> Char
    head' = head

    munge :: (x -> y) -> (y -> (w,z)) -> x -> w
    munge j k p = fst $ k (j p)