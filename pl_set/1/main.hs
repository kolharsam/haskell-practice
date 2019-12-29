my_sum :: [Integer] -> Integer
my_sum [] = 0 
my_sum (x:xs) = x + my_sum xs

isEven :: Integer -> Bool
isEven a = 
    if a `mod` 2 == 0
        then True
        else False
    
evenSum :: [Integer] -> Integer
evenSum [] = 0
evenSum (x:xs) =
    if isEven x
        then x + evenSum xs
        else evenSum xs

odd_sum :: [Integer] -> Integer
odd_sum li =
    case li of
        [] -> 0
        x:xs -> do
            if isEven x
                then odd_sum xs
                else x + odd_sum xs

increment :: Integer -> Integer
increment x = x + 1

decrement :: Integer -> Integer
decrement x = x - 1

alternate_sum :: [Integer] -> Integer -> Integer -> Integer
alternate_sum li i res =
    case li of
        [] -> res
        (x:xs) -> case isEven i of
                    True  -> alternate_sum xs (increment i) (res + x)
                    False -> alternate_sum xs (decrement i) (res - x)


main = do
    print (my_sum [1,2,3,4])
    print (isEven 3)
    print (evenSum [1,2,3,4])
    print (odd_sum [1,2,3,4])
    print (alternate_sum [1,2,3,4] 0 0)
