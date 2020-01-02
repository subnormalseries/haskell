module TopOrLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
    where woot :: Integer
          woot = 10
          topLevelValue = 5



area d = pi * (r ^ 2)
    where r = d / 2
    
--datatypes = types
-- type constructors and data constructors