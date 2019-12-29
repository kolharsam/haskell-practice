-- Problem Statement
-- Find the last but one element in the list


lastButOne :: [a] -> a
lastButOne li = do
  case li of
    [] -> error "No Result"
    [_] -> error "No Result"
    [x, _] -> x
    (_:y:xs) -> lastButOne (y:xs)

lastButOne2 :: [a] -> a
lastButOne2 = head . tail . reverse

-- Other methods : https://wiki.haskell.org/99_questions/Solutions/2

main :: IO ()
main = do
  putStrLn $ show (lastButOne [1,2,3])
  putStrLn $ show (lastButOne "Hello, world!")
  putStrLn $ show (lastButOne ['a', 'b', 'c', 'g'])
  putStrLn $ show (lastButOne2 ['a', 'b', 's'])
