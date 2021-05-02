{-# LANGUAGE InstanceSigs #-}
module Chapter16 where

--class Functor f where

--fmap :: (a -> b) -> f a -> f b

class Sumthin a where
    s :: a -> a


-- Here b is the first argument to the (->) type constructor, which has kind * -> * so
-- b has kind *
-- f is the result type of the (->) type constructor so f(g a b c) has type *
-- f has one type parameter so it is (* -> *)
-- in Haskell functions are applied to arguments
-- g is applied to 3 arguments so :k of g is (* -> * -> * -> *)
--class Else where
 --   e :: b -> f (g a b c)


{-
Slayer has kind (* -> (* -> *) -> (* -> *) -> *) 
e (* -> * -> *)
a, b, c, d have kind *

-}
--class Biffy where
--    slayer :: e a b -> (a -> c) -> (b -> d) -> e c d


data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
    -- add InstanceSigs at top of file to allow type signature in typeclass instance declaration
    fmap :: (a -> b) -> FixMePls a -> FixMePls b
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)
    -- important to note that the f here is a function from a -> b and has nothing
    -- to do with the Functor f in the fmap type signature
 -- it doesn't make sense as FixMePLs doesn't have a type constraint and is of kind *
-- Tbe best we can get for fmap is (FixMePls -> FixMePls) -> FixMePls -> FixMePls
-- Which is (a -> a) -> a -> a which is just function application
-- Functor is function application


data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)
instance Functor WhoCares where
    fmap :: (a -> b) -> WhoCares a -> WhoCares b
    fmap f (Matter a) = Matter (f a)
    fmap _ ItDoesnt = ItDoesnt
    fmap _ WhatThisIsCalled = WhatThisIsCalled


data CountingBad a = Heisenberg Int a deriving (Eq, Show)

instance Functor CountingBad where
    fmap :: (a -> b) -> CountingBad a -> CountingBad b
    -- Can't increase the value of the int or else it will break the composobility functor law
    fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)

oneWhoKnocks = Heisenberg 0 "Uncle" :: CountingBad String

jesse = fmap (++ " Jesse") oneWhoKnocks
lol = fmap (++ " lol") jesse
jesseLol = fmap ((++ "Jesse") . (++ " lol")) oneWhoKnocks


replaceWithP :: a -> Char
replaceWithP = const 'p'




lms :: [Maybe String]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe String] -> Char
replaceWithP' = const 'p'

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- The functor f is a []
liftedReplace' :: [Maybe String] -> String
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f2) => f1 (f2 a) -> f1 (f2 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe String] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted
  :: (Functor f1, Functor f2, Functor f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe String] -> [Maybe String]
thriceLifted' = thriceLifted

main :: IO ()
main = do
  putStr "replaceWithP lms: "
  print (replaceWithP' lms)

  putStr "liftedReplace lms: "
  print (liftedReplace lms)

  putStr "liftedReplace' lms: "
  print (liftedReplace' lms)

  putStr "twiceLifted lms: "
  print (twiceLifted lms)

  putStr "twiceLifted' lms: "
  print (twiceLifted' lms)

  putStr "thriceLifted lms: "
  print (thriceLifted lms)

  putStr "thriceLifted' lms: "
  print (thriceLifted' lms)

a :: [] Int
a = (+ 1) <$> (read "[1]" :: [Int])

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi", "Hello"])

c :: Num a => a -> a
c = (* 2) . (\x -> x - 2)

d :: Integer -> String
d = (("1" ++) . show) . (\x -> [x, 1 .. 3])

e :: IO Integer
e =
  let ioi     = readIO "1" :: IO Integer
      changed = read . ("123" ++) . show <$> ioi :: IO Integer
  in  fmap (* 3) changed


data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b
