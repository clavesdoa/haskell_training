myLength :: [a] -> Int

myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myMean :: [Double] -> Double

myMean [] = 0
myMean xs = mySum xs / fromIntegral (myLength xs)
       where mySum [] = 0
             mySum (a:xs) = (a + mySum xs)

