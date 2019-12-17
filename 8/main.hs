split_up :: [Integer] -> ([Integer], [Integer]) -> ([Integer], [Integer])
split_up li (res_odd, res_even) =
    case li of
        [] -> (res_odd, res_even)
        (x:xs) -> do
            case x `mod` 2 == 0 of
                True -> split_up xs (res_odd, (res_even ++ [x]))
                False -> split_up xs ((res_odd ++ [x]), res_even)


main = do
    print (split_up [1,2,3,4,5,6,7,8,9,3453,234,23,62,623,21] ([], []))