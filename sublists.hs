subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = subs xs ++ [ x:ys | ys <- subs xs ]