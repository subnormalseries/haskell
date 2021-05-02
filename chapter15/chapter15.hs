module Chapter15 where

import           Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

-- As of April 2018 Semigroup is a superclass of Monoid
-- The minimal complete definition for a Monoid typeclass now only requires mempty

instance Monoid a => Monoid (Optional a) where
  mempty                    = Nada

-- If however you want to mappend then this is defined in the superclass Semigroup
instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada Nada         = Nada
  (<>) (Only x) (Only y) = Only (x <> y)
  (<>) (Only x) Nada     = Only x
  (<>) Nada     (Only y) = Only y

answer :: Optional (Sum Integer)
answer = Only (Sum 1) <> Only (Sum 2)

answer' :: Optional (Product Integer)
answer' = Only (Product 4) <> Only (Product 5)

evilPlus :: (Num a) => a -> a -> a
evilPlus = flip (+)

evilPlusPlus :: [a] -> [a] -> [a]
evilPlusPlus = flip (++)

oneList = [1 .. 3]
otherList = [4 .. 6]

-- Typeclasses have a UNIQUE pairing between 
-- typeclass and the instance for a particular type


newtype First' a = First' { getFirst' :: Optional a} deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' { getFirst' = Nada}

instance Semigroup (First' a) where
  (<>) (First' { getFirst' = Nada }) (First' { getFirst' = Nada }) = First' { getFirst' = Nada }
  (<>) (First' { getFirst' = Nada }) (First' { getFirst' = Only y }) = First' { getFirst' = Only y }
  (<>) (First' { getFirst' = Only x }) _ = (First' { getFirst' = Only x })
