-- Numbers into words
module WordNumber where
import           Data.List                      ( intersperse )


digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "enter a digit 0 - 9"


-- takes a number and split into constituent parts
-- e.g. 123 -> [1, 2, 3]
digits :: Int -> [Int]
digits 0 = []
digits n = (digits (div n 10)) ++ (mod n 10) : []

-- 123 -> [1, 2, 3] -> ["one", "two", "three"]
wordNumber :: Int -> String
wordNumber = hyphenate . (map digitToWord) . digits

-- Takes ["one", "two", "three"] as a parameter
-- Returns "one-two-three"
hyphenate :: [String] -> String
hyphenate (x : []) = x
hyphenate (x : xs) = x ++ "-" ++ hyphenate xs
