-- Problem Statement : An extended version of FizzBuzz -> where the numbers are not just
-- 3 and 5 but instead the user provides input for the digits and their corresponding phrases.


-- import System.Environment                                    -- for getArgs method to get input as command line arguments 
import qualified Data.Map as Map
-- import Data.Typeable                                         -- for typeOf to see the varible's type at runtime
import Control.Monad
import Data.List

type MyPair = (Integer, String)

-- provides current input as a number
seekNumber :: IO Integer
seekNumber = do
  input <- getLine
  return ((read :: String -> Integer) $ input)

-- To collect input for the different magic phrases & their associated multiples.
seekSets :: IO MyPair
seekSets = do
  input <- getLine
  let extracts = words input
  let num = (read :: String -> Integer) $ (extracts !! 0)
  let phrase = (extracts !! 1)
  return (num, phrase)

-- this method is used to take user inputs repeatedly and provide the output as
-- an array of type myPair 
readLines :: Integer -> IO [MyPair]
readLines 0 = return []
readLines n = do
  x <- seekSets
  rest <- readLines (n-1)
  return $ x : rest

-- tests whether a particular number is divisible by a number from the map
testBuzz :: Integer -> Integer -> Bool
testBuzz test num = do
  case test `mod` num of
    0 -> True
    otherwise -> False

-- it is my implementation of the join to concat an array of Strings.
myJoin :: [String] -> String
myJoin li = Data.List.intercalate "" li


main = do
  -- Input the number of digits
  n <- seekNumber
  -- Input the number of magic phrases that are there
  m <- seekNumber
  -- Input the numbers and their respective phrases
  phrases <- readLines m
  -- Make map of the phrases from list
  let newMap = Map.fromList phrases
  -- this is where the magic happens. might not be the most idiomatic way to do it in Haskell.
  -- this does the job for now. but I have TOO many questions that I need answers for.
  forM_ [1..n] $ \x -> do
    result <- forM (Map.toList newMap) $ \(num, phrase) -> do
      if testBuzz x num
        then return phrase
        else return ""
    if (myJoin result) == ""
      then putStrLn (show x)
      else putStrLn (myJoin result)

