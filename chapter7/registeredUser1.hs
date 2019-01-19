module RegisteredUser where

addOne x = x + 1

addOne' = \x -> x + 1

addOneIfOdd :: (Integral a) => a -> a
addOneIfOdd n = case odd n of
  True -> addOne n
  _    -> n

addOneIfOdd' = \x -> case odd x of
  True -> f x
  _    -> x
  where f n = n + 1

--   This is invalid syntax as lambda's cannot have guards
--   addOneIfOdd'' = \x -> 
--     | odd x = x + 1
--     | otherwise = x

addOneIfOdd'' x | x > 3     = x + 1
                | otherwise = x

addOneIfOdd''' x = if odd x then (+ 1) x else x

addFive :: (Num a, Ord a) => a -> a -> a
addFive x y = (if x > y then y else x) + 5

addFive' = \x -> \y -> (if x > y then y else x) + 5

addFive'' = \x -> \y -> case x > y of
  True  -> y + 5
  False -> x + 5

mflip :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
mflip f = \x -> \y -> f y x

mflip' f x y = f y x




newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = UnRegisteredUser | RegisteredUser Username AccountNumber


-- printUser :: User -> IO ()
-- printUser UnRegisteredUser = putStrLn "UnregisteredUser"
-- printUser (RegisteredUser (Username name) (AccountNumber number)) =
--   putStrLn ("RegisteredUser Name: " ++ show name ++ " Number: " ++ show number)

printUser :: User -> IO ()
printUser UnRegisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username username) (AccountNumber accountNumber)) =
  putStrLn
    (  "Registered User Name: "
    ++ show username
    ++ " AccountNumber: "
    ++ show accountNumber
    )


data WherePenguinsLive = Galapagos | Antarctica | Australia | SouthAfrica | SouthAmerica deriving (Eq, Show)

data Penguin = Penguin WherePenguinsLive deriving Show

isSouthAfrican :: WherePenguinsLive -> Bool
isSouthAfrican SouthAfrica = True
isSouthAfrican _           = False


gimmieWhereTheyLive' :: Penguin -> WherePenguinsLive
gimmieWhereTheyLive' (Penguin whereTheyLive) = whereTheyLive

isSouthAfricanPenguin :: Penguin -> Bool
isSouthAfricanPenguin (Penguin SouthAfrica) = True
isSouthAfricanPenguin _                     = False

-- isSouthAfrica :: Penguin -> Bool
-- isSouthAfrica (Penguin SouthAfrica) = True
-- isSouthAfrica _                     = False

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False


gimmieWhereTheyLive :: Penguin -> WherePenguinsLive
gimmieWhereTheyLive (Penguin place) = place


galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Penguin Galapagos) = True
galapagosPenguin _                   = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Penguin Antarctica) = True
antarcticPenguin _                    = False




antarcticOrGalapagosPenguin :: Penguin -> Bool
antarcticOrGalapagosPenguin peng =
  antarcticPenguin peng || galapagosPenguin peng
