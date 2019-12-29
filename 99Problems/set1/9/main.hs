-- Problem Statement
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

-- Examples
-- (pack '(a a a a b c c a a d e e e e))
-- ((A A A A) (B) (C C) (A A) (D) (E E E E))


pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:first) : (pack rest) where
  myPack [] = ([], [])
  myPack (y:ys)
      | y == x = let (f, r) = myPack ys in (y:f, r)
      | otherwise = ([], (y:ys))
  (first, rest) = myPack xs

twoPack :: Eq a => [a] -> [[a]]
twoPack [] = []
twoPack [x] = [[x]]
twoPack (x:xs) = if x `elem` (head (twoPack xs))
  then ((x:(head (twoPack xs))):(tail (twoPack xs)))
  else ([x]:(twoPack xs))

-- More nice solutions on here: https://wiki.haskell.org/99_questions/Solutions/9

main :: IO ()
main = do
  putStrLn $ show (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e'])
  putStrLn $ show (twoPack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e'])
