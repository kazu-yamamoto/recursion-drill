-- % cabal install hspec
-- % runghc <this_file>

import Nat
import Small

main :: IO ()
main = hspec $ do
    describe "my_plus_n" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> my_from_n (my_to_n m `my_plus_n` my_to_n n) == m + n
    describe "my_minus_n" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= n && n >= 0
        ==> my_from_n (my_to_n m `my_minus_n` my_to_n n) == m - n
    describe "my_mul_n" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 1
        ==> my_from_n (my_to_n m `my_mul_n` my_to_n n) == m * n
    describe "my_lt_n" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> my_to_n m `my_lt_n` my_to_n n == (m < n)
    describe "my_divide_n" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 1
        ==> my_from_n (my_to_n m `my_divide_n` my_to_n n) == m `div` n
    describe "my_remainder_n" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 1
        ==> my_from_n (my_to_n m `my_remainder_n` my_to_n n) == m `mod` n

my_plus_n :: Nat -> Nat -> Nat
my_plus_n m n
  | my_isZero_n n = m
  | otherwise     = my_plus1_n (m `my_plus_n` my_minus1_n n)

my_isOne_n :: Nat -> Bool
my_isOne_n n
  | my_isZero_n n               = False
  | my_isZero_n (my_minus1_n n) = True
  | otherwise                   = False

my_mul_n :: Nat -> Nat -> Nat
my_mul_n m n
  | my_isOne_n n = m
  | otherwise    = my_mul_n m (my_minus1_n n) `my_plus_n` m

my_minus_n :: Nat -> Nat -> Nat
my_minus_n = undefined

my_lt_n :: Nat -> Nat -> Bool
my_lt_n = undefined

my_divide_n :: Nat -> Nat -> Nat
my_divide_n = undefined

my_remainder_n :: Nat -> Nat -> Nat
my_remainder_n = undefined
