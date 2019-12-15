 -- NOTE: problem incomplete. ask for help

rep :: Integer -> Integer -> [Integer] -> [Integer]
rep li reps res =
    case reps of
        0 -> []
        reps -> rep li (reps - 1) res ++ [li]

res = []
repeats :: ([Integer], [Integer]) -> [Integer]
repeats (xs, reps) =
    case xs of
        [] -> res
        (x:xys) -> do
            res ++ rep x (head reps) []
            repeats (xys, (drop 1 reps))


-- printArr :: Show a => [a] -> IO()
-- printArr putStrLn . concat . map show 


main = do
    print (rep 4 12 [])
    print (repeats ([1, 2, 3], [4, 0, 2]))
    -- printArr res