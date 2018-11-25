module Mood where

data Mood = Blah | Woot deriving Show
{-
This is a multiline comment in haskell.
This function is using pattern matching
-}
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah
