-- Problem Statement
-- Eliminate consecutive duplicates of list elements.

-- Example : (compress '(a a a a b c c a a d e e e e))
-- expected output: (A B C A D E)

-- Some neat solutions in here: https://wiki.haskell.org/99_questions/Solutions/8
-- idiomaticity! (yes! that's my own word!) go get it!

compress :: Eq a => [a] -> [a]
compress li = myCompress (tail li) [(li !! 0)] (li !! 0) where
  myCompress [] res _ = reverse res
  myCompress (x:xs) res l = do
    if x == l
      then myCompress xs res x
      else myCompress xs ([x] ++ res) x

main :: IO ()
main = do
  putStrLn $ show (compress [1,2,2,2,1,1,1])
  print (compress ['a', 'b', 'b', 'c', 'c', 'c', 'd', 'd'])
  print (compress "aaaabccaadeeee")
