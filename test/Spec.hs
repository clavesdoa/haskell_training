-- A tiny dependency-free test runner: each check is a name plus a Bool.
import Control.Monad (unless)
import System.Exit (exitFailure)

import qualified DigitOps as D
import Edinburgh
import ExpressionTrees
import List
import MyLength (myMean)
import MySecond
import Palindrome
import Parser (parse, parseInt)
import SafeListFuns
import SplitWith
import SquareSums
import Sublists
import Translation

checks :: [(String, Bool)]
checks =
  [ ("myEnumFromTo",     myEnumFromTo 3 6 == [3, 4, 5, 6])
  , ("factorial",        factorial 5 == 120 && factorialRec 5 == 120)
  , ("dot",              dot [1, 2, 3] [4, 5, 6 :: Int] == 32)
  , ("search",           search "banana" 'a' == [1, 3, 5])
  , ("mySum",            mySum [1 .. 10] == 55)
  , ("evalExp",          evalExp e0 == 11 && evalExp e1 == 15)
  , ("showExp",          showExp e0 == "(2+(3*3))")
  , ("satisfiable",      satisfiable p1 && not (satisfiable p0))
  , ("List round trip",  toList (fromList "abc") == "abc")
  , ("myMean",           myMean [1, 2, 3, 4] == 2.5)
  , ("tidySecond",       tidySecond "ab" == Just 'b' && tidySecond "a" == Nothing)
  , ("isPalindrome",     isPalindrome "abba" && not (isPalindrome "abc"))
  , ("parseInt",         parse parseInt "-42" == -42)
  , ("safeLast",         safeLast [1, 2, 3 :: Int] == Just 3 && safeHead "" == Nothing)
  , ("splitWith",        splitWith (/= ' ') "hello  world" == ["hello", "world"])
  , ("sumSqOddRec",      sumSqOddRec [1, 2, 3] == 10)
  , ("subs",             length (subs "abc") == 8)
  , ("delem",            delem (dList dChar) "b" ["a", "b"])
  , ("DigitOps",         D.digitToInt '7' == 7 && D.toUpper 'q' == 'Q')
  ]

main :: IO ()
main = do
  let failures = [name | (name, ok) <- checks, not ok]
  mapM_ (putStrLn . ("FAIL " ++)) failures
  putStrLn (show (length checks - length failures) ++ "/" ++ show (length checks) ++ " checks passed")
  unless (null failures) exitFailure
