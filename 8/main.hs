split_up :: [Integer] -> ([Integer], [Integer])
split_up li = split_list li ([], []) where
    split_list [] (res_odd, res_even) = (res_odd, res_even)
    split_list (x:xs) (res_odd, res_even) = do
        if even x
            then split_list xs (res_odd, (res_even ++ [x]))
            else split_list xs ((res_odd ++ [x]), res_even)



main = do
    print (split_up [1,2,3,4,5,6,7,8,9,3453,234,23,62,623,21])