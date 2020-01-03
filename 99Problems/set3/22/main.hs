-- Problem Statement
-- Create a list containing all integers within a given range

-- Example:
-- range 4 9
-- Output : [4,5,6,7,8,9]

range :: Int -> Int -> [Int]
range a b = do
  if b > a
    then [a..b]
    else [b..a]

-- Here we're assuming that b > a
range' :: Int -> Int -> [Int]
range' a b = range_maker a [] where
  range_maker p res = do
    if p == b
      then reverse (p:res)
      else range_maker (p+1) (p:res)

range'' :: Int -> Int -> [Int]
range'' a b = do
  if a <= b
    then range_maker a b []
    else range_maker b a []
    where
      range_maker start end acc = do
        if start == end
          then reverse (end:acc)
          else range_maker (start+1) end (start:acc)



main :: IO ()
main = do
  print (range 4 9)
  print (range' 4 9)
  print (range'' 9 9)
