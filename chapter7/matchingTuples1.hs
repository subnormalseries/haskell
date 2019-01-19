module TupleFunctions where

addEmUp2 :: (Num a) => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2' :: (Num a) => (a, a) -> a
addEmUp2' t = fst t + snd t


k :: (a, b) -> a
k (x, y) = x

k1 = k ((4 - 1), 10) :: Integer
k2 = k ("three", (1 + 2)) :: [Char]

k3 :: Integer
k3 = k (3, True)

f :: (a, b, c) -> (d, e, g) -> ((a, d), (c, g))
f (a, _, c) (d, _, g) = ((a, d), (c, g))

-- funcZ x = if (==) ((+ 1) x) 1 then "Awesome" else "Wut"
-- funcZ x = if x + 1 == 1 then "Awesome" else "Wut"
funcZ :: Integer -> [Char]
funcZ x = case x + 1 == 1 of
  True  -> "Awesome"
  False -> "Wut"

palindrome :: [Char] -> [Char]
palindrome xs = case y of
  True  -> "Palindrome"
  False -> "Not a palindrome"
  where y = (==) xs $ reverse xs

functionC :: (Ord a) => a -> a -> a
functionC x y = if (x > y) then x else y

functionC' :: (Ord a) => a -> a -> a
functionC' x y = case (x > y) of
  True -> x
  _    -> y

ifEvenAdd2 :: Integer -> Integer
ifEvenAdd2 n = if even n then n + 2 else n

ifEvenAdd2' :: Integer -> Integer
ifEvenAdd2' n = case even n of
  True  -> n + 2
  False -> n

nums :: (Ord a, Num a) => a -> a
nums x = case compare x 0 of
  GT -> 1
  LT -> -1
  EQ -> 0


data Employee = Coder | Manager | Veep | CEO deriving (Ord, Show, Eq)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' = case compare e e' of
  GT -> reportBoss e e'
  LT -> reportBoss e' e
  EQ -> putStrLn "Neither Employee is the boss"


employeeRank'
  :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank' h e e' = case h e e' of
  GT -> reportBoss e e'
  LT -> reportBoss e' e
  EQ -> putStrLn "Coders are equal"



codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _     = GT
codersRuleCEOsDrool _     Coder = LT
codersRuleCEOsDrool e     e'    = compare e e'


dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

bloodNa :: Integer -> String
bloodNa x | x < 135   = "too low"
          | x > 145   = "too high"
          | otherwise = "just right"


isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c | a ^ 2 + b ^ 2 == c ^ 2 = "Right angled triangle"
              | otherwise              = "Not right angled trianlge"


dogYears :: Integer -> Integer
dogYears x | x <= 0    = 0
           | x <= 1    = x * 15
           | x <= 2    = x * 12
           | x <= 4    = x * 8
           | otherwise = x * 6

avgGrade :: (Fractional a, Ord a) => a -> [Char]
avgGrade x | otherwise = "F"
           | y >= 0.9  = "A"
           | y >= 0.8  = "B"
           | y >= 0.7  = "C"
           | y >= 0.59 = "D"
           | y < 0.59  = "F"
  where y = x / 100

pal :: (Eq a) => [a] -> Bool
pal xs | xs == reverse xs = True
       | otherwise        = False

numbers :: (Num a, Ord a, Num p) => a -> p
numbers x | x < 0  = -1
          | x == 0 = 0
          | x > 0  = 1



g :: Int -> [Int] -> Int
g z xs = foldr (+) z xs



-- Reviewing Currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appendCatty :: String -> String
appendCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- woops mrow woohoo!
-- appendCatty "woohoo!"

-- 1 mrow haha 
-- frappe "1"

-- 
-- frappe (appendCatty "2")
-- frappe (cattyConny "woops" "2")
-- frappe "woops mrow 2"
-- flippy "haha" "woops mrow 2"
-- flip cattyConny "haha" "woops mrow 2"
-- cattyConny "woops mrow 2" "haha"
-- "woops mrow 2 mrow haha"

-- appendCatty (frappe "blue")
-- appendCatty "blue mrow haha"
-- "woops mrow blue mrow haha"

-- cattyConny (frappe "pink") (cattyConny "grenn" (appendCatty "blue"))
-- "pink mrow haha mrow green mrow woops mrow blue"

sumRecursively :: (Eq a, Num a) => a -> a
sumRecursively 1 = 1
sumRecursively n = n + sumRecursively (n - 1)


multiplicationRecursively :: (Integral a) => a -> a -> a
multiplicationRecursively n 1 = n
multiplicationRecursively x y = x + multiplicationRecursively x (y - 1)




