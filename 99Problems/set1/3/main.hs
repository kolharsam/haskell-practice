-- Problem Statement
-- Get k'th element of a list


element_at :: [a] -> Int -> a
element_at li pos = (li !! (pos - 1))     -- pos - 1 since indexing starts from 1

element_at2 :: [a] -> Int -> a
element_at2 li pos = finder li (pos - 1) 0 where
  finder [] _ _ = error "No Result"
  finder (x:xs) po pointer = do
    if po == pointer
      then x
      else finder xs (po) (pointer+1)


-- More methods at : https://wiki.haskell.org/99_questions/Solutions/3

main :: IO ()
main = do
  putStrLn $ show (element_at [1,2,3,4] 2)
  putStrLn $ show (element_at "Hello, World!" 4)
  putStrLn $ show (element_at2 [1,2,3,4,5,6,7,8,9] 6)
  putStrLn $ show (element_at2 ['a', 'b', 'c', 'd'] 3)
