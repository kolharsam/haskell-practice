-- Problem Statement
-- 19. Rotate a list N places to the left
-- 20. Remove the kth element from the list

-- Some more solutions for each at : https://wiki.haskell.org/99_questions/Solutions/20
-- https://wiki.haskell.org/99_questions/Solutions/19


remove' :: Eq a => [a] -> Int -> (a, [a])
remove' li pos = ((li !! (pos-1)), ((take (pos - 1) li) ++ (drop pos li)))

rotate :: Eq a => [a] -> Int -> [a]
rotate li num = do
  if num >= 0
    then (drop num li) ++ (take num li)
    else let g = ((length li) + num) in ((drop g li) ++ (take g li))


main :: IO ()
main = do
  print (rotate [1,2,3,4,5,6,7] 2)
  print (rotate "abcdefghi" (-4))
  print (remove' [1,2,3,4,5,6] 3)
  print (remove' "abcd" 2)
