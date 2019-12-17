-- import Debug.Trace                                                   -- very useful while debugging within functions!

my_zip :: ([Integer], [Integer]) -> [(Integer, Integer)]
my_zip (a, b) = zipper a b [] where
    zipper a [] res = res
    zipper [] b res = res
    zipper (x:xs) (y:ys) res = zipper xs ys (res ++ [(x,y)])


my_cyclic_zip :: ([Integer], [Integer]) -> [(Integer, Integer)]
my_cyclic_zip (a, b) = cyclic_zipper a b [] where
    cyclic_zipper [] b res = res
    cyclic_zipper a [] res = cyclic_zipper a b res
    cyclic_zipper (x:xs) (y:ys) res = cyclic_zipper xs ys (res ++ [(x,y)])



main = do
    print (my_zip ([1,2,4], [6,234]))
    print (my_cyclic_zip ([1,2,4], [6,234]))
