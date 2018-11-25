data Trivial = Trivial'

instance Eq Trivial where
    (==) Trivial' Trivial' = True


-- data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Eq

data DayOfWeek =  Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show

instance Eq DayOfWeek        where
    (==) Mon Mon = True
    (==) Tue Tue =  True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

data Date = Date DayOfWeek Int deriving Show

instance Eq Date where
    (==) (Date weekDay dayOfMonth) (Date weekDay' dayOfMonth') =
        (==) weekDay weekDay' &&
        (==) dayOfMonth dayOfMonth'


data Identity a = Identity a deriving Show

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = (==) v v'


data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) = x == y


data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') = x == x' && y == y'

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt y) = (==) x y
    (==) (TisAString x) (TisAString y) = (==) x y
    (==) _ _ = False


data Pair a = Pair a a deriving Show

instance Eq a => Eq (Pair a) where
    Pair x y == Pair x' y' = x == x' && y == y'


data Tuple a b = Tuple a b deriving Show

instance (Eq a, Eq b) => Eq (Tuple a b) where
    Tuple x y == Tuple x' y' = x == x' && y == y'

data Which a = ThisOne a | ThatOne a deriving Show

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne x) (ThatOne y) = x == y
    (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b deriving Show

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello _) (Hello _) = True
    (==) (Goodbye _) (Goodbye _) = True
    (==) _ _= False
