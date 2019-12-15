cum_sum_list :: [Integer] -> [Integer] -> Integer -> [Integer]
cum_sum_list li res res_num =
    case li of
        [] -> res
        (x:xs) -> cum_sum_list xs (res ++ [(res_num + x)]) (res_num+x)

main = do
    print (cum_sum_list [1,2,3,4,5,6,7,8,9,10] [] 0)

