import Data.List

data Exp = Lit Int
         | Add Exp Exp
         | Mul Exp Exp


evalExp :: Exp -> Int
evalExp (Lit n) = n
evalExp (Add e f) = evalExp e + evalExp f
evalExp (Mul e f) = evalExp e * evalExp f


showExp :: Exp -> String
showExp (Lit n) = show n
showExp (Add e f) = par (showExp e ++ "+" ++ showExp f)
showExp (Mul e f) = par (showExp e ++ "*" ++ showExp f)

par :: String -> String
par s = "(" ++ s ++ ")"

e0, e1 :: Exp
e0 = Add (Lit 2) (Mul (Lit 3) (Lit 3))
e1 = Mul (Add (Lit 2) (Lit 3)) (Lit 3)

type Name = String
data Prop = Var Name
         | F
         | T
         | Not Prop
         | Prop :|: Prop
         | Prop :&: Prop
         deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name,Bool)]

showProp :: Prop -> String
showProp (Var x) = x
showProp F = "F"
showProp T = "T"
showProp (Not p) = par ("~" ++ showProp p)
showProp (p :|: q) = par (showProp p ++ "|" ++ showProp q)
showProp (p :&: q) = par (showProp p ++ "&" ++ showProp q)

names :: Prop -> Names
names (Var x) = [x]
names F = []
names T = []
names (Not p) = names p
names (p :|: q) = nub (names p ++ names q)
names (p :&: q) = nub (names p ++ names q)

eval :: Env -> Prop -> Bool
eval e (Var x) = lookUp e x
eval e F = False
eval e T = True
eval e (Not p) = not (eval e p)
eval e (p :|: q) = eval e p || eval e q
eval e (p :&: q) = eval e p && eval e q

lookUp :: Eq a => [(a,b)] -> a -> b
lookUp xys x = the [ y | (x',y) <- xys, x == x' ]
      where
      the [x] = x

p0 :: Prop
p0 = (Var "a" :&: Not (Var "a"))

env0 :: Env
env0 = [("a",True)]

p1 :: Prop
p1 = (Var "a" :&: Var "b") :|: (Not (Var "a") :&: Not (Var "b"))

env1 :: Env
env1 = [("a",False), ("b",False)]

envs :: Names -> [Env]
envs [] = [[]]
envs (x:xs) = [ (x,b):e | b <- bs, e <- envs xs ]
           where
           bs = [False,True]

satisfiable :: Prop -> Bool
satisfiable p = or [ eval e p | e <- envs (names p) ]