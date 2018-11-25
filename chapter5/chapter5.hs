-- Listing all the functions with type signature Bool -> Bool
--not
--id
true :: Bool -> Bool
true _ = True

false :: Bool -> Bool
false _ = False

-- This function is non commutative
subtractStuff :: Integer -> (Integer -> Integer)
subtractStuff x y = x - y - 10

subtractOne = subtractStuff 1


-- Uncurrying a function
-- Firstly I have overridden the + function
-- (+) :: Num a => a -> a -> a
-- (+) x y = x Prelude.+ y

--Uncurried
(+) :: Num a => (a, a) -> a
(+) (x, y) = x Prelude.+ y
-- This isn't an infix operator
-- Example usage (Main.+) (5, 6)



nonsense :: Bool -> Integer
nonsense True = 805
nonsense _    = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i Prelude.+ (nonsense b)

unCurriedFunction :: (Integer, Bool) -> Integer
unCurriedFunction (i, b) = i Prelude.+ (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i Prelude.+ (nonsense b)

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i Prelude.+ (nonsense b)


--Generic curry
curry f a b = f (a, b)

--Generic uncurry
uncurry f (a, b) = f a b


f :: a -> a -> a -> a -> a; f = undefined
x :: Char; x = undefined

--Implementations of a -> b -> b
g :: a -> b -> b
g _ y = y

