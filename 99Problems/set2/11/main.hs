-- Problem Statement
-- Modified Run-Length encoding

-- Example : encodeModified "aaaabccaadeeee"
-- Output : [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

-- More Solutions here: https://wiki.haskell.org/99_questions/Solutions/13

-- Problem 13 is also similar and the solution of problem 12 is also here.

-- However you'd see some extra methods which are repetivtive, that was done for Problem 10 of Advent of Code
-- I couldn't solve it though, but yes, I should be coming back to it later. I did solve it using python

twoPack :: Eq a => [a] -> [[a]]
twoPack [] = []
twoPack [x] = [[x]]
twoPack (x:xs) = if x `elem` (head (twoPack xs))
  then ((x:(head (twoPack xs))):(tail (twoPack xs)))
  else ([x]:(twoPack xs))

i2pack :: [Int] -> [[Int]]
i2pack [] = []
i2pack [x] = [[x]]
i2pack (x:xs) = if x `elem` (head (i2pack xs))
  then ((x:(head (i2pack xs))):(tail (i2pack xs)))
  else ([x]:(i2pack xs))

lengthCount :: Eq a => [a] -> (Int, a)
lengthCount [] = (0, ([] !! 0))
lengthCount li = count li 0 where
  count [] res = (res, (li !! 0))
  count (_:xs) res = count xs (res+1)

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode li = map lengthCount (twoPack li)

ilenc :: [Int] -> (Int, Int)
ilenc [] = (0, 0)
ilenc li = count li 0 where
  count [] res = (res, (li !! 0))
  count (_:xs) res = count xs (res+1)

iencode :: [Int] -> [(Int, Int)]
iencode [] = []
iencode li = map ilenc (i2pack li)

data ModifiedItem a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [ModifiedItem a]
encodeModified li = map encode' (encode li) where
  encode' (1, x) = Single x
  encode' (n, x) = Multiple n x

decode :: [(Int, a)] -> [a]
decode = concatMap (uncurry replicate)

detuple :: [(Int, Int)] -> [Int]
detuple li = detup li [] where
  detup [] res = res
  detup ((x,y):xs) res = detup xs (res ++ [x,y])

newEncode :: [Int] -> Int -> [(Int, Int)]
newEncode li x = do
  case x of
    0 -> iencode li
    otherwise -> newEncode (detuple (iencode li)) (x-1)

main :: IO ()
main = do
  putStrLn $ show (encodeModified ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e'])
  putStrLn $ show (encodeModified [1, 1, 1, 1, 2, 3, 3, 4, 5, 5, 6, 6, 7])
  putStrLn $ show (encode [1,3,2,1,1,3,1,1,1,2])
  putStrLn $ show (length (newEncode [1,3,2,1,1,3,1,1,1,2] 39))
