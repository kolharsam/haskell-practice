-- Problem Statement
-- Find number of elements in a list

myLength :: [a] -> Int
myLength li = len li 0 where
  len [] res = res
  len [_] res = (res+1)               -- this line is redundant.
  len (_:xs) res = do
    len xs (res+1)

myLength2 :: [a] -> Int
myLength2 = sum . map (\_ -> 1)

myLength3 :: [a] -> Int
myLength3 [] = 0
myLength3 (_:xs) = 1 + myLength3 xs

-- More solutions at : https://wiki.haskell.org/99_questions/Solutions/4

main :: IO ()
main = do
  putStrLn $ show (myLength "Hello")
  putStrLn $ show (myLength [1,2,3,4])
  putStrLn $ show (myLength2 [1,2,3,4])
  putStrLn $ show (myLength3 [1,2,3,4])
