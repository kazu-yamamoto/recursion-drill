-- % cabal install hspec
-- % runghc <this_file>

import Nat
import Small

main :: IO ()
main = hspec $ do
    describe "plus" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> toInt (fromInt m `plus` fromInt n) == m + n
    describe "minus" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= n && n >= 0
        ==> toInt (fromInt m `minus` fromInt n) == m - n
    describe "mul" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 1
        ==> toInt (fromInt m `mul` fromInt n) == m * n
    describe "lt" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> fromInt m `lt` fromInt n == (m < n)
    describe "divide" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 1
        ==> toInt (fromInt m `divide` fromInt n) == m `div` n
    describe "remainder" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 1
        ==> toInt (fromInt m `remainder` fromInt n) == m `mod` n

plus :: Nat -> Nat -> Nat
plus m n
  | isZero n  = m
  | otherwise = plus1 (m `plus` minus1 n)

minus :: Nat -> Nat -> Nat
minus m n
  | isZero n  = m
  | otherwise = minus1 (m `minus` minus1 n)

isOne :: Nat -> Bool
isOne n
  | isZero n          = False
  | isZero (minus1 n) = True
  | otherwise         = False

mul :: Nat -> Nat -> Nat
mul m n
  | isOne n   = m
  | otherwise = mul m (minus1 n) `plus` m

lt :: Nat -> Nat -> Bool
lt m n
  | isZero n = False
  | isZero m = True
  | otherwise = lt (minus1 m) (minus1 n)

divide :: Nat -> Nat -> Nat
divide m n
  | m `lt` n  = zero
  | otherwise = plus1 ((m `minus` n) `divide` n)

remainder :: Nat -> Nat -> Nat
remainder m n
  | m `lt` n  = m
  | otherwise = remainder (m `minus` n) n
