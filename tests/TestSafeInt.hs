{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Main where

import Test.Framework as TF
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit as T
import Test.QuickCheck hiding ((===))
import Data.SafeInt
import Data.Word
import Data.List
import Data.Maybe
import Control.Exception as E

main :: IO ()
main = defaultMain tests

isArithException :: SafeInt -> IO Bool
isArithException n = E.catch (n `seq` return False)
                             (\ (_ :: ArithException) -> return True)

sameAsInteger :: (forall a. Integral a => a) -> Bool
sameAsInteger n = toInteger (n :: Int) == (n :: Integer)

behavesOk :: (forall a. Integral a => a) -> IO Bool
behavesOk n = if sameAsInteger n then fromIntegral (n :: Int) === n
                                 else isArithException n

unitTest :: Assertable t => TestName -> t -> TF.Test
unitTest msg p = testCase msg (T.assert p)

infix 1 ===
(===) :: SafeInt -> SafeInt -> IO Bool
x === y = return (x == y)

wordSize :: Int
wordSize = fromJust (find (\ n -> 2 ^ n == (0 :: Word)) [8,16,32,64,128])

tests :: [TF.Test]
tests =
  [ unitTest "0"       (0 + 0 === 0),
    unitTest "max+"    (isArithException (maxBound + 1)),
    unitTest "min-"    (isArithException (minBound - 1)),
    unitTest "1/0"     (isArithException (1 `div` 0)),
    unitTest "min*-1"  (isArithException (minBound * (-1))),
    unitTest "min/-1"  (isArithException (minBound `div` (-1))),
    unitTest "max/2*2" ((maxBound `div` 2) * 2 === maxBound - 1),
    unitTest "max+min" (maxBound + minBound === -1),
    unitTest "max+*"   (isArithException (2 ^ (wordSize `div` 2) * 2 ^ (wordSize `div` 2 - 1))),
    unitTest "min-*"   (negate (2 ^ (wordSize `div` 2)) * 2 ^ (wordSize `div` 2 - 1) === minBound),
    testProperty "*"   (propBinOp (*)),
    testProperty "+"   (propBinOp (+)),
    testProperty "-"   (propBinOp (-)),
    testProperty "div" (propBinOp div),
    testProperty "mod" (propBinOp mod),
    testProperty "quot"(propBinOp quot),
    testProperty "rem" (propBinOp rem),
    testProperty "lcm" (propBinOp lcm),
    testProperty "gcd" (propBinOp gcd)
  ]

anyInt :: Gen Int
anyInt = choose (minBound, maxBound)

propBinOp :: (forall a. Integral a => a -> a -> a) -> Property
propBinOp (!) = forAll anyInt $ \ x ->
                forAll anyInt $ \ y ->
                ioProperty $
                behavesOk (fromIntegral x ! fromIntegral y)

