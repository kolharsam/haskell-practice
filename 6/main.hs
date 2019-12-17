split_up :: [Integer] -> ([Integer], [Integer])
split_up li = split_arr li ([], []) where
    split_arr [] (res_a, res_b) = (res_a, res_b)
    split_arr (x:xs) (res_a, res_b) = do
        if x > 0
            then split_arr xs ((res_a ++ [x]), res_b)
            else split_arr xs (res_a, (res_b ++ [x]))

split_at :: [Integer] -> Integer -> ([Integer], [Integer])
split_at li a = split_arr li ([], []) where
    split_arr [] (res_a, res_b) = (res_a, res_b)
    split_arr (x:xs) (res_a, res_b) = do
        if x > a
            then split_arr xs ((res_a ++ [x]), res_b)
            else split_arr xs (res_a, (res_b ++ [x]))


main = do
    print (split_up [-1,-2,3,4,-5,6,7,8,-9,10,11,-12,13,-14])
    print (split_at [-1,-2,3,4,-5,6,7,8,-9,10,11,-12,13,-14] 9)

    -- HOW DO I MAKE IT WORK WITHOUT ME GIVING THE INITIAL EMPTY STATE???
    -- Comments like the one I've mentioned above really help me to learn!
    -- Thank God for the amazing ways I use comments like these to learn.