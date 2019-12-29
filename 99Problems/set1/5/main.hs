-- Problem Statement
-- Reverse a list

myRev :: [a] -> [a]
myRev li = reverser li [] where
  reverser [] rev = rev
  reverser (x:xs) rev = reverser xs ([x] ++ rev)

-- Other Solution : https://wiki.haskell.org/99_questions/Solutions/5


main :: IO ()
main = do
  putStrLn $ show (myRev [1,2,3,4])
