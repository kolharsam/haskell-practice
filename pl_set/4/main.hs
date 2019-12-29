rep :: Integer -> Integer -> [Integer] -> [Integer]
rep li reps res =
    case reps of
        0 -> []
        reps -> rep li (reps - 1) res ++ [li]

repeats :: ([Integer], [Integer]) -> [Integer]
repeats (li, reps) = repeat_arr li reps [] where
    repeat_arr [] reps res_arr = res_arr
    repeat_arr li [] res_arr = res_arr
    repeat_arr (x:xs) (y:ys) res_arr = repeat_arr xs ys (res_arr ++ (rep x y []))


main = do
    print (repeats ([1, 2, 3], [4, 0, 2]))
    
