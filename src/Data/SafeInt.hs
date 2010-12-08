-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SafeInt
-- Copyright   :  (c) 2010 Well-Typed LLP
-- License     :  BSD3
-- 
-- Maintainer  :  Andres Loeh <andres@well-typed.com>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Defines a variant of Haskell's Int type that is overflow-checked. If
-- an overflow or arithmetic error occurs, a run-time exception is thrown.
--
--------------------------------------------------------------------------

{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.SafeInt (SafeInt(..), fromSafe, toSafe) where

import GHC.Prim
import GHC.Base
import GHC.Err
import GHC.Num
import GHC.Word
import GHC.Real
import GHC.Types

newtype SafeInt = SI Int

fromSafe :: SafeInt -> Int
fromSafe (SI x) = x

toSafe :: Int -> SafeInt
toSafe x = SI x

instance Show SafeInt where

  showsPrec p x = showsPrec p (fromSafe x)

instance Read SafeInt where

  readsPrec p xs = [ (toSafe x, r) | (x, r) <- readsPrec p xs ]

instance Eq SafeInt where

  (==) = eqSafeInt
  (/=) = neSafeInt

{-# INLINE eqSafeInt #-}
eqSafeInt :: SafeInt -> SafeInt -> Bool
eqSafeInt (SI (I# x#)) (SI (I# y#)) = x# ==# y#

{-# INLINE neSafeInt #-}
neSafeInt :: SafeInt -> SafeInt -> Bool
neSafeInt (SI (I# x#)) (SI (I# y#)) = x# /=# y#

instance Ord SafeInt where

  SI x <  SI y = x <  y
  SI x <= SI y = x <= y
  SI x >  SI y = x >  y
  SI x >= SI y = x >= y

instance Num SafeInt where

  (+)               = plusSI
  (*)               = timesSI
  (-)               = minusSI
  negate x@(SI y)
    | x == minBoundSafe = overflowError
    | otherwise         = SI (negate y)
  abs x
    | x >= 0        = x
    | otherwise     = negate x
  signum x | x > 0  = 1
  signum 0          = 0
  signum _          = -1
  fromInteger x
    | x > maxBoundInteger || x < minBoundInteger
                    = overflowError
    | otherwise     = SI (fromInteger x)

maxBoundInteger :: Integer
maxBoundInteger = toInteger (maxBound :: Int)

minBoundInteger :: Integer
minBoundInteger = toInteger (minBound :: Int)

minBoundSafe :: SafeInt
minBoundSafe = SI minBound

maxBoundSafe :: SafeInt
maxBoundSafe = SI maxBound

instance Bounded SafeInt where

  minBound = minBoundSafe
  maxBound = maxBoundSafe

instance Enum SafeInt where

  succ x
    | x /= maxBoundSafe = x `plusSI` 1
    | otherwise         = succError "SafeInt"
  pred x
    | x /= minBoundSafe = x `minusSI` 1
    | otherwise         = predError "SafeInt"
  toEnum                = SI
  fromEnum              = fromSafe
  enumFrom              = integralEnumFrom
  enumFromThen          = integralEnumFromThen
  enumFromTo            = integralEnumFromTo
  enumFromThenTo        = integralEnumFromThenTo

instance Integral SafeInt where

  quot (SI x) (SI y) = SI (quot x y)
  rem (SI x) (SI y)  = SI (rem x y)
  div (SI x) (SI y)  = SI (div x y)
  mod (SI x) (SI y)  = SI (mod x y)
  quotRem (SI x) (SI y) = case quotRem x y of
                            (q,r) -> (SI q, SI r)
  divMod (SI x) (SI y) = case divMod x y of
                           (d,m) -> (SI d, SI m)
  toInteger (SI x) = toInteger x

instance Real SafeInt where

  toRational (SI x) = toInteger x % 1

plusSI :: SafeInt -> SafeInt -> SafeInt
plusSI (SI (I# x#)) (SI (I# y#)) =
  case addIntC# x# y# of
    (# r#, 0# #) -> SI (I# r#)
    (# _ , _  #) -> overflowError

minusSI :: SafeInt -> SafeInt -> SafeInt
minusSI (SI (I# x#)) (SI (I# y#)) =
  case subIntC# x# y# of
    (# r#, 0# #) -> SI (I# r#)
    (# _ , _  #) -> overflowError

timesSI :: SafeInt -> SafeInt -> SafeInt
timesSI (SI (I# x#)) (SI (I# y#)) =
  case mulIntMayOflo# x# y# of
    0# -> SI (I# (x# *# y#))
    _  -> overflowError

{-# RULES
"fromIntegral/Int->SafeInt" fromIntegral = toSafe
  #-}

sumS :: [SafeInt] -> SafeInt
sumS     l       = sum' l 0
  where
    sum' []     a = a
    sum' (x:xs) a = sum' xs (a + x)

productS :: [SafeInt] -> SafeInt
productS l       = prod l 1
  where
    prod []     a = a
    prod (x:xs) a = prod xs (a*x)

{-# RULES
  "sum/SafeInt"          sum = sumS;
  "product/SafeInt"      product = productS
  #-}

{-# RULES
  "sum/SafeInt"          sum = sumS;
  "product/SafeInt"      product = productS
  #-}

lcmS :: SafeInt -> SafeInt -> SafeInt
lcmS _ 0         =  0
lcmS 0 _         =  0
lcmS x y         =  abs ((x `quot` (gcdS x y)) * y)

gcdS :: SafeInt -> SafeInt -> SafeInt
gcdS 0 0 = error "GHC.Real.gcdInt: gcd 0 0 is undefined"
gcdS a b = fromIntegral (gcdInteger (fromIntegral a) (fromIntegral b))

{-# RULES
  "lcm/SafeInt"          lcm = lcmS;
  "gcd/SafeInt"          gcd = gcdS
  #-}
