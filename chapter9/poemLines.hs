module PoemLines where

import           ListStuff

firstSen = "Tyger, Tyger buring bright\n"
secondSen = "In the forest of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

{-
I don't even understand this.
What is happening after the first recursive iteration. Because variables are
immutable I would expect xs to remain the same throughout

Obviously not you fuck wit.
Takewhile and dropwhile use the same predicate. Not equalling the line character.
dropWhile (/y -> y /= '\n') sentences leaves a string whose first character is \n
-}
myLines :: String -> [String]
myLines []          = []
myLines ('\n' : xs) = myLines xs
myLines xs          = takeWhile (/= '\n') xs : myLines (dropWhile (/= '\n') xs)


shouldEqual =
  [ "Tyger, Tyger buring bright"
  , "In the forest of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]


main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
