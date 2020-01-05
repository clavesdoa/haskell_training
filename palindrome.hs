isOdd :: Int -> Bool

isOdd n
   | mod n 2 == 1 = True
isOdd _ = False

myLength :: [a] -> Int

myLength [] = 0
myLength (_:xs) = 1 + myLength xs


myReverse :: [a] -> [a]

myReverse [] = []
myReverse (a:xs) = myReverse xs ++ (a:[])

palindrome :: [a] -> [a]

palindrome [] = []
palindrome xs = xs ++ myReverse xs

isPalindrome :: Eq a => [a] -> Bool

isPalindrome xs
        | isOdd (myLength xs) == True = False
        | xs == myReverse xs = True
isPalindrome _ = False
