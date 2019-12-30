-- Problem Statement
-- Duplicate elements of list


dupli :: Eq a => [a] -> [a]
dupli li = dup li [] where
  dup [] res = reverse res
  dup (x:xs) res = dup xs (x:x:res)

main :: IO ()
main = do
  print (dupli [1,2,3,4,5])
  print (dupli ['a', 'b', 'c'])
