divide_array :: [Integer] -> ([Integer], [Integer]) -> ([Integer], [Integer])
divide_array li (res_a, res_b) = 
    case li of
        [] -> (res_a, res_b)
        (x:xs) -> do
            if length xs `mod` 2 /= 0
                then divide_array xs ((res_a ++ [x]), res_b)
                else divide_array xs (res_a, (res_b ++ [x]))


main = do
    print (divide_array [1,2,3,4,5,6,7,8,9,10,11,12,13,14] ([], []))

    -- HOW DO I MAKE IT WORK WITHOUT ME GIVING THE INITIAL EMPTY STATE???