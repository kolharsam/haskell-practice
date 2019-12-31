-- Problem Statement
-- Split a list into two parts; the length of the first part is given

-- Example
-- split "abcdefghik" 3
-- Output: ("abc", "defghik")

splitAt' :: Eq a => [a] -> Integer -> ([a], [a])
splitAt' li num = splitter li 1 [] where
  splitter [] _ res = (res, [])
  splitter (x:xs) count res = do
    case count == num of
      True -> ((res ++ [x]), xs)
      False -> splitter xs (count + 1) (res ++ [x])


main :: IO ()
main = do
  print (splitAt' "abcdefghik" 3)
