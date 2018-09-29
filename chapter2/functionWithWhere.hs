module FunctionWithWhere where

printInc n = print plusTwo
  where plusTwo = n + 2

printInc' n = let plusTwo = n + 2
              in print plusTwo