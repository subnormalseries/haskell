module MadLib where

import           Data.Monoid
import Test.QuickCheck

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madLibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madLibbin' e adv noun adj =
  e
    <> "! he said "
    <> adv
    <> " as he jumped into his car "
    <> noun
    <> " and drove off with his "
    <> adj
    <> " wife."

madLibbin'' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madLibbin'' e adv n adj = mconcat
  [ e
  , "! he said "
  , adv
  , " as he jumped into his car "
  , n
  , "and drove off with his"
  , adj
  , " wife"
  ]

-- A lambda that tests associativity of a function f
-- \f a b c -> a `f` (b `f` c) == (a `f` b) `f` c

asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c = (a (<>) b) <> c == a <> (b <> c)



