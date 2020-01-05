safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (_:xs) = Just xs
safeTail [] = Nothing

safeLast :: [a] -> Maybe a
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs
safeLast [] = Nothing

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)