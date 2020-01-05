splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p xs = shift p [] [] xs

shift :: (a -> Bool) -> [[a]] -> [a] -> [a] -> [[a]]
shift p [[]] [] [] = []
shift p acc1 [] [] = acc1
shift p acc1 acc2 [] = appendNotEmpty acc1 acc2
shift p acc1 acc2 (x:xs) | p x == False = shift p (appendNotEmpty acc1 acc2) []  xs
                         | otherwise = shift p acc1 (acc2 ++ [x]) xs 

appendNotEmpty :: [[a]] -> [a] -> [[a]]
appendNotEmpty [[]] [] = []
appendNotEmpty xs [] = xs
appendNotEmpty [] ys = [ys]
appendNotEmpty xs ys = xs ++ [ys]
