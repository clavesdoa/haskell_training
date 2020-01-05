myEnumFromTo :: Int -> Int -> [Int]

myEnumFromTo m n | m > n = []
                 | otherwise = m : myEnumFromTo (m + 1) n

factorial :: Int -> Int

factorial n = product [1..n]

factorialRec :: Int -> Int

factorialRec n | n <= 0 = 1
               | otherwise = n * factorialRec (n-1)

myZip :: [a] -> [b] -> [(a,b)]

myZip [] ys = []
myZip xs [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys 

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum [ x*y | (x,y) <- myZip xs ys ]

search :: Eq a => [a] -> a -> [Int]
search xs y = [ i | (i,x) <- zip [0..] xs, x==y ]

selectComp :: [a] -> Int -> a -- (!!)
selectComp xs i = the [ x | (j,x) <- zip [0..] xs, j == i ]
        where
        the [x] = x

takeComp :: Int -> [a] -> [a]
takeComp i xs = [ x | (j,x) <- zip [0..] xs, j < i ]

dropComp :: Int -> [a] -> [a]
dropComp i xs = [ x | (j,x) <- zip [0..] xs, j >= i ]

squares :: [Int] -> [Int]
squares xs = map sqr xs
         where
         sqr x = x*x

myFoldr :: (a -> a -> a) -> a -> [a] -> a
myFoldr f v [] = v
myFoldr f v (x:xs) = f x (myFoldr f v xs)

mySum :: [Int] -> Int
mySum = myFoldr (+) 0

data Nat = Zero
          | Succ Nat

power :: Float -> Nat -> Float
power x Zero = 1.0
power x (Succ n) = x * power x n