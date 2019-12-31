-- Problem Statement
-- Rotate a list N places to the left

rotate :: Eq a => [a] -> Int -> [a]
rotate li num = do
  if num >= 0
    then (drop num li) ++ (take num li)
    else let g = ((length li) + num) in ((drop g li) ++ (take g li))


main :: IO ()
main = do
  print (rotate [1,2,3,4,5,6,7] 2)
  print (rotate "abcdefghi" (-4))
