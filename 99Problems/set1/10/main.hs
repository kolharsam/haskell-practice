-- Problem Statement
-- Perform Run Length Encoding on a given list

-- Example:
-- encode "aaaabccaadeeee"
-- Output: [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

twoPack :: Eq a => [a] -> [[a]
                          ]
twoPack [] = []
twoPack [x] = [[x]]
twoPack (x:xs) = if x `elem` (head (twoPack xs))
  then ((x:(head (twoPack xs))):(tail (twoPack xs)))
  else ([x]:(twoPack xs))

lengthCount :: Eq a => [a] -> (Integer, a)
lengthCount [] = (0, ([] !! 0))
lengthCount li = count li 0 where
  count [] res = (res, (li !! 0))
  count (_:xs) res = count xs (res+1)

encode :: Eq a => [a] -> [(Integer, a)]
encode [] = []
encode li = map lengthCount (twoPack li)


-- more solutions can be found here


main :: IO()
main = do
  putStrLn $ show (encode ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e'])
  putStrLn $ show (encode [1, 1, 1, 1, 2, 3, 3, 4, 5, 5, 6, 6, 7])
