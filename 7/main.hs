isSorted :: [Integer] -> Bool
isSorted li = 
    checkSorted li False
    where
        checkSorted [] res = res
        checkSorted [x] res = res
        checkSorted (x:y:xs) res = do
            if x < y
                then checkSorted (y:xs) True
                else False                            

main = do
    print (isSorted [1,2,3,4,5,6,7])

