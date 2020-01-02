module Chapter13 where
import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

twoo :: IO Bool
twoo = do
    c <- getChar
    c' <- getChar
    return (c == c')

main :: IO ()
main = do 
    c <- getChar
    c' <- getChar
    if (c == c') then
        putStrLn "True"
    else 
        return ()

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    let line1Lower = map toLower line1
    case (equality line1Lower (reverse line1Lower)) of
        True -> do putStrLn "It's a palindrome"
        _ -> do putStrLn "Nope!"
                exitSuccess
    where equality word1 word2 = 
            (==) (filter (isAlpha) word1) (filter (isAlpha) word2)



type Name = String
type Age = Integer

data Person = Person Name Age 

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String 
        deriving (Eq, Show)


instance Show Person where
    show (Person name age) = ("Name: " ++ name) ++ (" Age: " ++ show age)
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
        | name /= "" && age > 0 = Right $ Person name age
        | name == "" && age > 0 = Left NameEmpty
        | name /= "" && age < 0 = Left AgeTooLow
        | otherwise = Left $ PersonInvalidUnknown $
            "Name was: " ++ show name ++
            " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "What is your name? "
    name <- getLine
    putStr "How old are you? "
    age <- getLine
    let person = mkPerson name (read age)
    validatePerson person

validatePerson :: Either PersonInvalid Person -> IO ()
validatePerson (Left NameEmpty) = do putStrLn "The persons name was empty"
validatePerson (Left AgeTooLow) = do putStrLn "The persons age was too low"
validatePerson (Left (PersonInvalidUnknown reason)) = do putStrLn $ "Invalid Person: " ++  reason
validatePerson (Right person) = do putStrLn $ "Yay successfully got a person " ++ (show person)
