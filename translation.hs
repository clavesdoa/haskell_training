import Data.Char

data EqDict a = EqD (a -> a -> Bool)

eq :: EqDict a -> a -> a -> Bool
eq (EqD f) = f

delem :: EqDict a -> a -> [a] -> Bool

-- comprehension
-- delem d x ys = or [ eq d x y | y <- ys ]

-- recursion
-- delem d x [] = False
-- delem d x (y:ys) = eq d x y || delem d x ys

-- higher-order
delem d x ys = foldr (||) False (map (eq d x) ys)

dInt :: EqDict Int
dInt = EqD eqInt
   where
   eqInt a b = a == b

dChar :: EqDict Char
dChar = EqD f
   where
   f x y = eq dInt (ord x) (ord y)

dPair :: (EqDict a, EqDict b) -> EqDict (a,b)
dPair (da,db) = EqD f
   where
   f (u,v) (x,y) = eq da u x && eq db v y

dList :: EqDict a -> EqDict [a]
dList d = EqD f
   where
   f [] [] = True
   f [] (y:ys) = False
   f (x:xs) [] = False
   f (x:xs) (y:ys) = eq d x y && eq (dList d) xs ys