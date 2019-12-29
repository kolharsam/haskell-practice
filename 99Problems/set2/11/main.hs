-- Problem Statement
-- Modified Run-Length encoding

-- Example : encodeModified "aaaabccaadeeee"
-- Output : [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

twoPack :: Eq a => [a] -> [[a]]
twoPack [] = []
twoPack [x] = [[x]]
twoPack (x:xs) = if x `elem` (head (twoPack xs))
  then ((x:(head (twoPack xs))):(tail (twoPack xs)))
  else ([x]:(twoPack xs))

lengthCount :: Eq a => [a] -> (Int, a)
lengthCount [] = (0, ([] !! 0))
lengthCount li = count li 0 where
  count [] res = (res, (li !! 0))
  count (_:xs) res = count xs (res+1)

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode li = map lengthCount (twoPack li)

data ModifiedItem a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [ModifiedItem a]
encodeModified li = map encode' (encode li) where
  encode' (1, x) = Single x
  encode' (n, x) = Multiple n x


main :: IO ()
main = do
  putStrLn $ show (encodeModified ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e'])
  putStrLn $ show (encodeModified [1, 1, 1, 1, 2, 3, 3, 4, 5, 5, 6, 6, 7])
