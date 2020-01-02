module Cipher where

import           Data.Char

-- Start off with a fixed caesar cipher of 3 and then 
-- refactor to replace 3 with a parameter
caesar :: Int -> [Char] -> [Char]
caesar n = map $ (chr . shift n)

shift :: Int -> Char -> Int
shift n x | x == ' '  = ord ' '
          | isUpper x = go 'A'
          | isLower x = go 'a'
 where
  go character =
    ord character + mod ((ord x `mod` ord character) + (n `mod` 26)) 26

uncaesar :: Int -> [Char] -> [Char]
uncaesar n x = caesar (negate n) x



