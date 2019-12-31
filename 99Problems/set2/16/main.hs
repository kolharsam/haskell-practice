-- Problem Statement
-- Drop every nth element from the list

dropEvery :: Eq a => [a] -> Integer -> [a]
dropEvery li num = drop' li 1 [] where
  drop' [] _ res = reverse res
  drop' (x:xs) count res = do
    case count `mod` num of
      0 -> drop' xs (count + 1) res
      otherwise -> drop' xs (count+1) (x:res)

-- plenty of solutions are available here: https://wiki.haskell.org/99_questions/Solutions/16

main :: IO ()
main = do
  print (dropEvery "abcdefghik" 3)
  print (dropEvery [1,2,3,4,5,6,7,8] 2)
