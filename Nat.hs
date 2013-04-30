module Nat (
    Nat
  , my_zero_n
  , my_isZero_n
  , my_plus1_n
  , my_minus1_n
  , my_from_n
  , my_to_n
  ) where

data Nat = Z | S Nat

my_zero_n :: Nat
my_zero_n = Z

my_isZero_n :: Nat -> Bool
my_isZero_n Z = True
my_isZero_n _ = False

my_plus1_n :: Nat -> Nat
my_plus1_n = S

my_minus1_n :: Nat -> Nat
my_minus1_n (S n) = n
my_minus1_n _     = error "my_minus1_n"

my_from_n :: Nat -> Integer
my_from_n n = iter n 0
  where
    iter m acc
      | my_isZero_n m = acc
      | otherwise     = iter (my_minus1_n m) (acc + 1)

my_to_n :: Integer -> Nat
my_to_n 0 = Z
my_to_n n = S (my_to_n (n - 1))

