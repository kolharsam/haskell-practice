-- Problem Statement
-- Say whether a list is palindrome

myReverse :: [a] -> [a]
myReverse li = rev li [] where
  rev [] res = res
  rev (x:xs) res = rev xs (x:res)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome li =  li == (myReverse li)


-- Many a fancy solutions found here: https://wiki.haskell.org/99_questions/Solutions/6

main :: IO ()
main = do
  putStrLn $ show (isPalindrome [1,2,1])
  putStrLn $ show (isPalindrome "madamimadam")
  putStrLn $ show (isPalindrome [1,2,3,4])
