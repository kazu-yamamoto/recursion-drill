module Nat (
    Nat
  , zero
  , isZero
  , plus1
  , minus1
  , toInt
  , fromInt
  ) where

data Nat = Z | S Nat

zero :: Nat
zero = Z

isZero :: Nat -> Bool
isZero Z = True
isZero _ = False

plus1 :: Nat -> Nat
plus1 n = S n

minus1 :: Nat -> Nat
minus1 (S n) = n
minus1 _     = error "minus1"

toInt :: Nat -> Int
toInt n = toInt' n 0
  where
    toInt' m acc
      | isZero m  = acc
      | otherwise = toInt' (minus1 m) (acc + 1)

fromInt :: Int -> Nat
fromInt 0 = Z
fromInt n = S (fromInt (n - 1))

