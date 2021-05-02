module Main where

import           Data.Monoid
import           Test.QuickCheck
import           Control.Monad
import           Data.List.NonEmpty
import           Data.Semigroup
import           GHC.Generics
main :: IO ()
main = do
  let ma  = monoidAssoc
      mli = monoidLeftId
      mri = monoidRightId
  -- here checking that our Bool copy data type is associative. It is
  --quickCheck (ma :: Bull -> Bull -> Bull -> Bool)
  -- here checking for left identity. It isn't due to the definition of mempty in the !!
  -- Monoid Bull instance definition
  --quickCheck (mli :: Bull -> Bool)
  -- same as above
  --quickCheck (mri :: Bull -> Bool)
  quickCheck (ma :: FirstMappend)
  quickCheck (mli :: FirstId)
  quickCheck (mri :: FirstId)
  quickCheck (trivailSemigroupAssoc :: TrivialAssoc)
  quickCheck (identitySemigroupAssoc :: IdentityAssoc)
  quickCheck (twoSemigroupAssoc :: TwoAssoc)
  quickCheck (fourSemigroupAssoc :: FourAssoc)
  quickCheck (boolconjSemigroupAssoc :: BoolConjAssoc)
  quickCheck (booldisjSemigroupAssoc :: BoolDisjAssoc)
  quickCheck (orSemigroupAssoc :: OrAssoc)
  quickCheck (propValidationAssoc)
  quickCheck (trivialIdentity)
  -- quickCheck (sa :: IdentityAssoc) -- Can't bind a value to a type more than once in scope
  -- So this errors :t sa has type TrivialAssoc


-- This is an invariant that should always be true
-- We aren't testing that a bool is returned as that is checked by the compiler
-- Here QuickCheck creates 3 random strings for each of the 100 test cases
-- passes them to the monoidAssoc function and checks that the return value is True.
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = ((a <> b) <> c) == (a <> (b <> c))

-- example quickcheck invariant
abs' :: (Num a, Ord a) => a -> a
abs' n | n < 0     = (-n)
       | otherwise = n

monoidLeftId :: (Eq m, Monoid m) => m -> Bool
monoidLeftId m = (mempty <> m) == m

monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId m = (m <> mempty) == m

leftIdWorks = quickCheck (monoidLeftId :: String -> Bool)
rightIdWorks = verboseCheck (monoidRightId :: Sum Int -> Bool)

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools

instance Semigroup Bull where
  -- our mappend function always returns Fools
  -- This example can be viewed as the Bool data type
  (<>) _ _ = Fools

-- This can't be a monoid because the identity fails


data Optional a = Nada | Only a deriving (Eq, Show)
-- Defined an instance of our Arbitrary typeclass. This is needed in order for quickCheck to randomly 
-- generate values of this type.
-- The type signature says that given a type a which has an instance of the Arbitrary typeclass
-- Then the arbitrary instance of our datatype First' a is defined inside of a do monad.

-- because the type a has an instance of the Arbitrary typeclass we can use the <- syntax to bind a value
-- to our parameter x. Where x :: a 
-- using frequency we can determine in what proportion these random values are selected.
-- here we have a 1:1 ratio between First' {getFirst' = Nada} and First' {getFirst' = Only x}
-- return wraps the First' {getFirst'} into the Gen Monad.
instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return First' {getFirst' = Nada}), (1, return First' { getFirst' = Only x})]

instance Monoid a => Monoid (Optional a) where
  mempty                    = Nada

-- If however you want to mappend then this is defined in the superclass Semigroup
instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada Nada         = Nada
  (<>) (Only x) (Only y) = Only (x <> y)
  (<>) (Only x) Nada     = Only x
  (<>) Nada     (Only y) = Only y

-- This is just my version of First, a Monoid on Maybe datatype
-- First' uses record syntax
-- getFirst' here is a value of type Optional a
newtype First' a = First' { getFirst' :: Optional a} deriving (Eq, Show)
-- Created our instance of the Monoid typeclass for this type
instance Monoid (First' a) where
  mempty = First' { getFirst' = Nada}
-- Similarly created our instance of the Semigroup typeclass
-- Works by taking the first non Nada type, if there is no non Nada then it selects Nada
instance Semigroup (First' a) where
  (<>) (First' { getFirst' = Nada }) (First' { getFirst' = Nada }) = First' { getFirst' = Nada }
  (<>) (First' { getFirst' = Nada }) (First' { getFirst' = Only y }) = First' { getFirst' = Only y }
  (<>) (First' { getFirst' = Only x }) _ = (First' { getFirst' = Only x })

-- The type signature of our quickcheck properties
-- These are used when calling quickCheck to check associatiity and identity above.
type FirstMappend = (First' String) -> (First' String) -> (First' String) -> Bool
type FirstId = First' String -> Bool

-- Semigroups are Monoids without the Identity constraint. This is not saying that
-- in a Semigroup there can't exist an identity element, just that it need no exist. This is whay
-- All Monoids are Semigroups, but not all semigroups are monoids. 

data NonEmpty a = a :| [a] deriving (Eq, Ord, Show)
-- :| is an infix data constructor. This data constructor is defined in Data.List.NonEmpty 
-- [a] could be empty :| takes 2 type arguments. It's a product of a and [a] so we always have
-- one value of type a

data P = Prefix Int String

data Q = Int :!!: String -- We just defined this data constructor now

data T a = Bool :|> Maybe a -- infix data constructors MUST start with a colon 
--data R = :!!: Int String -- doesn't work as non alphanumeric data constructors are infix by default

-- data S = Int Prefix String -- doesn't work as alphanumeric data constructors are prefix by default.
-- Use newtype to create a type wrapper. Introduce a renaming to an algebraic data type
newtype NonEmpty' a = NonEmpty (a, [a]) deriving (Eq, Ord, Show) -- recall product types can be written using tuple syntax

-- Can't write a Monoid for the Non Empty data type as it has no identity
-- There is no mempty value such that for all t in the type t <> mempty = t for mappend operation <>

-- There is a binary associative operation that is closed so NonEmpty a can be a Semigroup

-- Chapter 15 Exercises
--1
data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
  (<>) Trivial Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Monoid Trivial where
  mempty = Trivial


trivailSemigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
trivailSemigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

-- type just provides an alias
-- This is used when calling quickCheck as it needs the type signatures to know what generators to call
-- to get the random values
type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
trivialIdentity :: Trivial -> Bool
trivialIdentity a = (a <> mempty == a) && (mempty <> a == a)


newtype Identity a = Identity a deriving (Eq, Show)
instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity (x <> y)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)
type IdentityAssoc = (Identity String) -> (Identity String) -> (Identity String) -> Bool
instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
identitySemigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
identitySemigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

data Two a b = Two a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)
instance (Monoid a, Monoid b) => Monoid(Two a b) where
  mempty = Two mempty mempty

twoSemigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
twoSemigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)


type TwoAssoc = (Two String Trivial) -> (Two String Trivial) -> (Two String Trivial) -> Bool


data Four a b c d = Four a b c d deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty

type FourAssoc = (Two String Trivial) -> (Two String Trivial) -> (Two String Trivial) -> Bool
fourSemigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
fourSemigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)



newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Semigroup (BoolConj) where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = frequency [(1, return (BoolConj True)), (1, return (BoolConj False))]

instance Monoid BoolConj where
  mempty = BoolConj True

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
boolconjSemigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
boolconjSemigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance Semigroup BoolDisj where
  BoolDisj True <> _ = BoolDisj True
  _ <> BoolDisj True = BoolDisj True
  _ <> _ = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = frequency [(10, return (BoolDisj False)), (1, return (BoolDisj True))]

instance Monoid BoolDisj where
  mempty = BoolDisj False

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
booldisjSemigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
booldisjSemigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

data Or a b = Fst a | Snd b deriving (Eq, Show)
instance Semigroup (Or a b) where
  Snd b <> _ = Snd b
  Fst a <> Snd b = Snd b
  Fst a <> Fst a' = Fst a'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return (Fst x)),  (2, return (Snd y))]

instance (Monoid a, Monoid b) => Monoid (Or a b) where
  mempty = Fst mempty

type OrAssoc = (Or String (Maybe String)) -> (Or String (Maybe String)) -> (Or String (Maybe String)) -> Bool
orSemigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
orSemigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

-- This is a record type with a function uncombine
newtype Combine a b = Combine { unCombine :: (a -> b)}
--instance Semigroup (Combine a b) where
 -- (<>) (Combine f) (Combine g) = (Combine )

newtype Comp a = Comp' { unComp :: (a -> a)}
-- The mappend operator here just composes the inner functions
instance Semigroup (Comp a) where
  (<>) (Comp' f) (Comp' g) = Comp' (f . g)

instance Show a => Show (Comp a) where
  show x = "Comp " ++ show x

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    x <- (arbitrary)
    return (Comp' x)



--instance Arbitrary (Comp a) where
--  arbitrary = do
--    x <- arbitrary
--    return

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Success' x) _ = Success' x
  (<>) _ (Success' y) = Success' y
  (<>) (Failure' x) (Failure' y) = Failure' (x <> y)

-- check using quickcheck
propValidationAssoc
  :: Validation String Integer
  -> Validation String Integer
  -> Validation String Integer
  -> Bool
propValidationAssoc x y z = (x <> y) <> z == x <> (y <> z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof [return (Failure' x), return (Success' y)]

instance (Monoid a) => Monoid (Validation a b) where
  mempty = Failure' mempty
