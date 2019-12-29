minim :: [Integer] -> Integer
minim li =
    case li of
        [] -> 0
        (x:[]) -> x
        (x:y:xs) -> do
            if x > y
                then minim (y:xs)
                else minim (x:xs)

maxim :: [Integer] -> Integer
maxim li =
    case li of
        [] -> 0
        (x:[]) -> x
        (x:y:xs) -> do
            if x > y
                then maxim (x:xs)
                else maxim (y:xs)

minmax :: [Integer] -> (Integer, Integer)
minmax li = ((minim li), (maxim li))

main = do
    print (minmax [1,2,3,4])
    print (minim [1,2,3,4])
    print (maxim [1,2,3,4])

