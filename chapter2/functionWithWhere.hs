module FunctionWithWhere where

printInc :: (Show a, Num a) => a -> IO ()
printInc n = print plusTwo
  where plusTwo = n + 2

printInc' :: (Show a, Num a) => a -> IO ()
printInc' n = let plusTwo = n + 2
              in print plusTwo

printInc'' n = print $ (+) 2 n