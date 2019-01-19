class Numberish a where 
    fromNumber :: Integer -> a
    toNumber :: a -> Integer
    defaultNumber :: a