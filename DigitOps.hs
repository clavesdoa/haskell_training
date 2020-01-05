isLower :: Char -> Bool
isLower x = 'a' <= x && x <= 'z'

isUpper :: Char -> Bool
isUpper x = 'A' <= x && x <= 'Z'

isDigit :: Char -> Bool
isDigit x = '0' <= x && x <= '9'

isAlpha :: Char -> Bool
isAlpha x = isLower x || isUpper x

digitToInt :: Char -> Int
digitToInt c | isDigit c = ord c - ord '0'

intToDigit :: Int -> Char
intToDigit d | 0 <= d && d <= 9 = chr (ord '0' + d)

toLower :: Char -> Char
toLower c | isUpper c = chr (ord c - ord 'A' + ord 'a')
          | otherwise = c

		  toUpper :: Char -> Char
toUpper c | isLower c = chr (ord c - ord 'a' + ord 'A')
          | otherwise = c