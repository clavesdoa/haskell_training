sumSqOddRec :: [Int] -> Int
sumSqOddRec [] = 0
sumSqOddRec (x:xs) | odd x = x*x + sumSqOddRec xs
                   | otherwise = sumSqOddRec xs