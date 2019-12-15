import Debug.Trace

mini_zip :: [Integer] -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
mini_zip a num res =
    case a of
        [] -> res
        (x:xs) -> mini_zip xs num (res ++ [(x, num)])

res = []
my_cyclic_zip :: ([Integer], [Integer]) -> [(Integer, Integer)]
my_cyclic_zip (a, b) = do
    case b of
        [] -> res
        (x:xs) -> do
            res <- res ++ [mini_zip a x []]
            trace ("my_cyclic_zip with res" ++ show res) (my_cyclic_zip (a,xs))
            my_cyclic_zip (a, xs)


my_zip :: ([Integer], [Integer]) -> [(Integer, Integer)]
my_zip (a, b) = do
    if length a > length b
        then case b of
                [] -> res
                (x:xs) -> do 
                    res <- [(head a, x)] ++ my_zip (tail a, xs)
        else case a of
                [] -> res
                (x:xs) -> do
                    res <- [(head b, x)] ++ my_zip (tail b, xs)



main = do
    print (mini_zip [1,2,3] 7 [])
    -- print (my_cyclic_zip ([1,2,3], [6,7]))
    print (my_zip ([1,2,4], [6,234]))
    print (zip [1,3,5] [23, 3])


-- Note:
-- when I do just this:

-- print (mini_zip [1,2,3] 7 [])
-- my_cyclic_zip ([1,2,3], [6,7])
-- I get the following error
-- main.hs:23:5: error:
--     • Couldn't match type ‘[]’ with ‘IO’
--       Expected type: IO (Integer, Integer)
--         Actual type: [(Integer, Integer)]
--     • In a stmt of a 'do' block: my_cyclic_zip ([1, 2, 3], [6, 7])
--       In the expression:
--         do print (mini_zip [1, 2, ....] 7 [])
--            my_cyclic_zip ([1, 2, ....], [6, 7])
--       In an equation for ‘main’:
--           main
--             = do print (mini_zip [1, ....] 7 [])
--                  my_cyclic_zip ([1, ....], [6, ....])
--    |
-- 23 |     my_cyclic_zip ([1,2,3], [6,7])