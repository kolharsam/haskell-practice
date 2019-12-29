-- Problem Statement :
-- Find the last element of a list


myLast :: [Integer] -> Integer
myLast li = do
  case li of
    [] -> error "No Result"
    [x] -> x
    (_:xs) -> myLast xs


myLast2 :: [a] -> a
myLast2 [x] = x
myLast2 (_:xs) = myLast2 xs

myLast3 :: [c] -> c
myLast3 = head . reverse


-- other methods : https://wiki.haskell.org/99_questions/Solutions/1

main :: IO ()
main = do
  print (myLast [1,2,3,4,5])
  print (myLast2 [1,2,3,6])
  print (myLast2 ['a', 'b', 'c'])
  print (myLast3 ['a', 'b', 'd'])
