module Mood where

data Mood = Blah | Woot deriving Show 
{-
This is a multiline comment in haskell.
This function is using pattern matching
-}
instance Ord Mood where 
    (>) Blah Woot = True
    (>) Woot Blah = False
    (>) _ _ = False

    (<=) Woot Blah = True
    (<=) Blah Woot = False
    (<=) _ _ = True


instance Eq Mood where 
    (==) Blah Blah = True
    (==) Woot Woot = True
    (==) _ _ = False


changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot    = Blah

bo :: Mood -> Bool
bo = (> Woot) 