-- How the IO monad works
module MyIO(MyIO, myPutChar, myGetChar, convert) where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)
import Data.Char

type Input = String
type Remainder = String
type Output = String

data MyIO a  =  MyIO (Input -> (a, Remainder, Output))

apply :: MyIO a -> Input -> (a, Remainder, Output)
apply (MyIO f) inp  =  f inp

myPutChar :: Char -> MyIO ()
myPutChar ch  =  MyIO (\inp -> ((), inp, [ch]))

myGetChar :: MyIO Char
myGetChar =  MyIO (\(ch:rem) -> (ch, rem, []))

instance Functor MyIO where
  fmap = liftM

instance Applicative MyIO where
  pure  = return
  (<*>) = ap

instance Monad MyIO where
  return x  =  MyIO (\inp -> (x, inp, ""))
  m >>= k   =  MyIO (\inp ->
                 let (x, rem1, out1) = apply m inp in
                 let (y, rem2, out2) = apply (k x) rem1 in
                 (y, rem2, out1++out2))

convert :: MyIO () -> IO ()
convert m  =  interact (\inp ->
                let (x, rem, out) = apply m inp in
                out)




