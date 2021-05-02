{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import           Test.QuickCheck
import           GHC.Arr


-- Gonna use quickcheck to test the functor laws for our functors

data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

{- 
The functor laws are identity and composability
fmap :: Functor f => (a -> b) -> f a -> f b
id <$> f a == f a <=> fmap id == id 
fmap id (f a) == f a
-}

type FunctorIdentity a = a -> Bool
type FunctorComposition c = c -> Bool

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity g = fmap id g == g

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap (g . f) x == fmap g (fmap f x)

main :: IO ()
main = do
  putStrLn "hello world"
  quickCheck (functorIdentity :: Maybe Integer -> Bool)
  let f = (+ 1)
  let g = (* 2)
  quickCheck (functorIdentity :: FunctorIdentity (Identity Integer))
  quickCheck (functorCompose f g :: FunctorComposition (Identity Integer))
  quickCheck (functorIdentity :: FunctorIdentity (Pair Integer))
  quickCheck (functorCompose f g :: FunctorComposition (Pair Integer))
  quickCheck (functorIdentity :: FunctorIdentity (Two' Integer Integer))
  quickCheck (functorCompose f g :: FunctorComposition (Two' Integer Integer))
  quickCheck
    (functorIdentity :: FunctorIdentity (Three Integer Integer Integer))
  quickCheck
    (functorCompose f g :: FunctorComposition (Three Integer Integer Integer))
  quickCheck (functorIdentity :: FunctorIdentity (Three' Integer Integer))
  quickCheck (functorCompose f g :: FunctorComposition (Three' Integer Integer))
  quickCheck
    (functorIdentity :: FunctorIdentity (Four Integer Integer Integer Integer))
  quickCheck
    (functorCompose f g :: FunctorComposition
        (Four Integer Integer Integer Integer)
    )
  quickCheck (functorIdentity :: FunctorIdentity (Four' Integer Integer))
  quickCheck (functorCompose f g :: FunctorComposition (Four' Integer Integer))

newtype Identity a = Identity a deriving (Eq, Show)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

data Two' a b = Two' a b deriving (Eq, Show)
instance Functor (Two' a) where
  fmap :: (b -> c) -> (Two' a)b -> (Two' a)c
  fmap f (Two' a b) = Two' a (f b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two' x y)


data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap :: (c -> d) -> Three a b c -> Three a b d
  fmap f (Three a b c) = Three a b (f c)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' a b c)  = Three' a (f b) (f c)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Three' x y y)


data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap :: (d -> e) -> Four a b c d -> Four a b c e
  fmap f (Four a b c d) = Four a b c (f d)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four w x y z)


data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Four' x x x y)

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust Nothing  = Nothing
incIfJust (Just n) = Just $ n + 1

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust Nothing  = Nothing
showIfJust (Just n) = Just $ show n

showIfJust' :: (Functor f, Show a) => f a -> f String
showIfJust' = fmap show

liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = (<$>) (+ 1)

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
instance Functor Possibly where
  fmap :: (a -> b) -> Possibly a -> Possibly b
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)


incIfRight :: Num b => Either a b -> Either a b
incIfRight (Left  x) = Left x
incIfRight (Right x) = Right (x + 1)


incEither :: Num b => Either a b -> Either a b
incEither = (<$>) (+ 1)

showEither :: Show b => Either a b -> Either a String
showEither = fmap show


data Sum' a b = First' a | Second' b deriving (Eq, Show)
instance Functor (Sum' a) where
  fmap :: (b -> c) -> Sum' a b -> Sum' a c
  fmap _ (First' b) = First' b
  fmap f (Second' a) = Second' $ f a

newtype Constant a b = Constant { getConstant :: a} deriving (Eq, Show)
instance Functor (Constant a) where
  fmap _ (Constant v) = Constant v

data Wrap f a = Wrap (f a) deriving (Eq, Show)
instance Functor f => (Functor (Wrap f)) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

getInt :: IO Integer
getInt = fmap read getLine

meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ " and me too !")

bumpIt :: IO Integer
bumpIt = do
  x <- getInt
  let y = x + 5
  return y

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)
instance Functor BoolAndSomethingElse where
  fmap f (False' x)=  False' (f x)
  fmap f (True' x) = True' (f x)

data BoolAndMaybeSomethingElse a = Falsish | Truish a
instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

-- Does this have a valid Functor instance

newtype Mu f = InF { outF :: f(Mu f) }
-- because f is not completely polymorphic we can see that it doesn't have kind *
-- The kindness of Mu is (* -> *) -> *
-- partially applying the type constructor like Tuple isn't going to work as the partially
--applied type constructor will have kind * not * -> * 

--data D = D (Array Word Word) Int Int
-- This cannot have a functor instance because the kindness of D is *

-- Here a is a phantom type, it doesn't appear after the = sign
-- This allows us to construct a functor instance
data D a = D (Array Word Word) Int Int

data Sum b a = First'' a | Second'' b deriving (Eq, Show)
instance Functor (Sum e) where
  fmap f (First'' a) = First'' (f a)
  fmap f (Second'' b) = Second'' b

data Company a c b = DeepBlue a c | Something b
instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap f (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)
instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

newtype K a b = K a
--instance Functor (K a) where
 -- fmap f (K a) = K a


--This is exactly how we defined a new Functor Instance for the Tuple type which allowed
-- transforming the first type parameter
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- Difference here between usual X a b = ..
-- both a and b are types. Here f is a higher kinded type with kind *
-- So I could think of f as some form of structure type like List, Maybe or Either
-- Have to partially apply the type constructor due to kind inference in the Functor instance
-- definition.
-- Had to add the typeclass constraint on f to allow fmapping over x
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)
instance (Functor f) => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = LiftItOut (fmap g x)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa x y) = DaWrappa (fmap h x) (fmap h y)




data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething x y) = IgnoringSomething x (fmap h y)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)


data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a x) = Cons (f a) (fmap f x)


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)





























