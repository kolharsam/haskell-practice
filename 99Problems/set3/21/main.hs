-- Problem Statement
-- Insert an element at a particular postion in an array

insert_at :: Eq a => [a] -> a -> Int -> [a]
insert_at li char pos = (first ++ [char] ++ rest) where
  first = take (pos-1) li
  rest = drop (pos-1) li

main :: IO ()
main = do
  print (insert_at [1,2,3,4,6,7,8,9] 5 5)
