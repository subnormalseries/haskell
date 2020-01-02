myLast :: [a] -> a
myLast = head . reverse

myButLast :: [a] -> a
myButLast = head . tail . reverse

elementAt :: [a] -> Int -> a
elementAt xs n = head . drop (n - 1) $ xs

myLength :: [a] -> Int
myLength = foldr (\_ n -> n + 1) 0

myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == myReverse x

-- Don't get confused here NestedList is a unary type constructor
-- The 2 data constructors/value constructor are Elem and List
-- Elem is a unary data constructor accepting a parameter of type a
-- So if we wanted a value of NestedList Bool we could construct that
-- value using the data constructor Elem True
-- The second data constructor is List. Again this is a unary data constructor
-- this time accepting a Prelude.List [] of type NestedList a
-- This NestedList a is the type in the type constructor.
-- I know you read that type constructors are found on the left of the equals
-- but that is just wrong.
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem x       ) = [x]
myFlatten (List (x : [])) = myFlatten x
myFlatten (List (x : xs)) = myFlatten x ++ myFlatten (List xs)
-- myFlatten (List (x : xs)) = myFlatten x ++ concat (map (myFlatten) xs)

compress :: Eq a => [a] -> [a]
compress []       = []
compress (x : []) = [x]
compress (x : xs) =
  if (x == head xs) then [x] ++ compress (tail xs) else [x] ++ compress xs

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs =
  takeWhile (\z -> z == head xs) xs : pack (dropWhile (\z -> z == head xs) xs)


encode :: Eq a => [a] -> [(Int, a)]
encode = map f . pack where f = \x -> (length x, head x)


data Encode a = Single a | Multiple Int a deriving Show

encodeModified :: Eq a => [a] -> [Encode a]
encodeModified xs = map f (encode xs)
  where f x = if (fst x == 1) then Single (snd x) else Multiple (fst x) (snd x)
