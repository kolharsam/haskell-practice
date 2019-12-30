-- Problem Statement
-- Replicate the elements for given number of times

makeCopies :: a -> Integer -> [a]
makeCopies str cop = copier str cop [] where
  copier str 0 res = reverse res
  copier str x res = copier str (x-1) (str:res)


repli :: Eq a => [a] -> Integer -> [a]
repli li times = repl li times [] where
  repl [] _ res = res
  repl (x:xs) _ res = repl xs times (res ++ (makeCopies x times))


main :: IO ()
main = do
  print (repli ['a', 'b', 'c'] 3)
  print (repli [1,2,3,4,5] 5)
