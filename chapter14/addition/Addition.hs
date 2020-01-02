module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3  `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 4 `shouldBe` (5, 2)
        it "5 multiplied by 4 is 20" $ do
            rm 5 4 `shouldBe` 20
        it "5 multiplied by -4 is -20" $ do
            rm 5 (negate 4) `shouldBe` (negate 20)
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Integer)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)


rm :: (Ord a, Eq a, Num a) => a -> a -> a
rm _ 0 = 0
rm 0 _ = 0
rm x y 
    | y > 0 = x + rm x (y - 1)
    | y < 0 = (-x) + rm x (y + 1)

trivialInt :: Gen Int
trivialInt = return 1

-- return :: Monad m => a -> m a
-- m is a type that has a monad typeclass instance
-- example monads include [], Maybe, IO, Either, Gen
-- return is used to put a value into a monad. Here it
-- is used to put the value 1 into the Gen monad.

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2,2,3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c) 

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]


genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

genMaybe' :: (Arbitrary a) => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [(1, return Nothing), (3, return (Just a))]