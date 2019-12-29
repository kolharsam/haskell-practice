data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- This is problem was not easy and was done using the hint : that we had to create a
-- new type such as NestedList and then solve it. I wonder if there is something like
-- the spread operator in Haskell. If not, then could we have it in the Haskell way?

-- Many many fancy solutions here: https://wiki.haskell.org/99_questions/Solutions/7

main :: IO ()
main = do
  putStrLn $ show (flatten (Elem 5))
  putStrLn $ show (flatten (List [Elem 5, Elem 4, List [Elem 2, List [Elem 3]], List [Elem 1]]))
