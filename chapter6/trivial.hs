data Trivial = Trivial'


instance Eq Trivial where
    (==) Trivial' Trivial' = True


-- data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Eq

data DayOfWeek =  Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Ord, Show)

instance Eq DayOfWeek        where
    (==) Mon Mon = True
    (==) Tue Tue =  True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

instance Ord DayOfWeek where 
    compare Fri Fri = EQ
    compare Fri _ = GT
    compare _ Fri = LT 
    compare _ _ = EQ

-- The Date data type has a nullary type constructor
-- It has a two arity data constructor
-- It accepts a DayOfWeek and an Int
-- Date :: DayOfWeek -> Int -> Date
data Date = Date DayOfWeek Int deriving Show


instance Eq Date where 
    (==) (Date dayOfWeek num) (Date dayOfWeek2 num2) = 
        dayOfWeek == dayOfWeek2 && num == num2



















data Identity a =  Identity a
        
instance Eq a => Eq (Identity a) where
    (==) (Identity x) (Identity x') = x == x'

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y


data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') = x == x' && y == y'

data StringOrInt = TisAnInt Int| TisAString String
instance Eq StringOrInt where 
    (==) (TisAnInt x) (TisAnInt x') = x == x'
    (==) (TisAString s) (TisAString s') = s == s'
    (==) _ _ = False


data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair x x') (Pair y y') =
        x == y && x' == y'













--data StringOrInt = TisAnInt Int | TisAString String
--
--instance Eq StringOrInt where
--    (==) (TisAnInt x) (TisAnInt y) = (==) x y
--    (==) (TisAString x) (TisAString y) = (==) x y
--    (==) _ _ = False
--
--
--data Pair a = Pair a a deriving Show
--
--instance Eq a => Eq (Pair a) where
--    Pair x $cshowy == Pair x' y' = x == x' && y == y'
--
--
--data Tuple a b = Tuple a b deriving Show
--
--instance (Eq a, Eq b) => Eq (Tuple a b) where
--      --    Tuple x y == Tuple x' y' = x == x' && y == y'
--
--data Which a = ThisOne a | ThatOne a deriving Show
--
--instance Eq a => Eq (Which a) where
--    (==) (ThisOne x) (ThisOne y) = x == y
--    (==) (ThatOne x) (ThatOne y) = x == y
--    (==) _ _ = False
--
--data EitherOr a b = Hello a | Goodbye b deriving Show
--
--instance (Eq a, Eq b) => Eq (EitherOr a b) where
--    (==) (Hello _) (Hello _) = True
--    (==) (Goodbye _) (Goodbye _) = True
--    (==) _ _= False
------------------------