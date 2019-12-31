-- Problem Statement
-- Extract a slice from a list.

-- slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- output : "cdefg"

-- some great solutions in here: https://wiki.haskell.org/99_questions/Solutions/18

slicer :: Eq a => [a] -> Integer -> Integer -> [a]
slicer li a b = elemslice li 1 [] where
  elemslice [] _ res = res
  elemslice (x:xs) pos res = do
    case pos > b of
      True -> res
      False -> do
        if pos >= a && pos <= b
          then elemslice xs (pos + 1) (res ++ [x])
          else elemslice xs (pos + 1) res

main :: IO ()
main = do
  print (slicer ['a','b','c','d','e','f','g','h','i','k'] 3 8)
