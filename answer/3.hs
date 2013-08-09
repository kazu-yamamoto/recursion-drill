-- % cabal install hspec
-- % runghc <this_file>

import Small

main :: IO ()
main = hspec $ do
    describe "my_lt" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> my_lt m n == (m < n)
    describe "my_lteq" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> my_lteq m n == (m <= n)
    describe "my_even" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> my_even n == even n
    describe "my_divide" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 1
        ==> my_divide m n == m `div` n
    describe "my_gt" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> my_gt m n == (m > n)
    describe "my_gteq" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> my_gteq m n == (m >= n)
    describe "my_odd" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> my_odd n == odd n
    describe "my_remainder" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 1
        ==> my_remainder m n == m `mod` n
    describe "my_divide_iter" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 1
        ==> my_divide_iter m n == m `div` n

my_lt :: Integer -> Integer -> Bool
my_lt _ 0 = False
my_lt 0 _ = True
my_lt m n = my_lt (m - 1) (n - 1)

my_lteq :: Integer -> Integer -> Bool
my_lteq 0 _ = True
my_lteq _ 0 = False
my_lteq m n = my_lteq (m - 1) (n - 1)

my_even :: Integer -> Bool
my_even 0 = True
my_even 1 = False
my_even n = my_even (n - 2)

my_divide :: Integer -> Integer -> Integer
my_divide m n
  | m < n     = 0
  | otherwise = my_divide (m - n) n + 1

my_gt :: Integer -> Integer -> Bool
my_gt 0 _ = False
my_gt _ 0 = True
my_gt m n = my_gt (m - 1) (n - 1)

my_gteq :: Integer -> Integer -> Bool
my_gteq _ 0 = True
my_gteq 0 _ = False
my_gteq m n = my_gteq (m - 1) (n - 1)

my_odd :: Integer -> Bool
my_odd 0 = False
my_odd 1 = True
my_odd n = my_odd (n - 2)

my_remainder :: Integer -> Integer -> Integer
my_remainder m n
  | m < n     = m
  | otherwise = my_remainder (m - n) n

my_divide_iter :: Integer -> Integer -> Integer
my_divide_iter x y = iter x y 0
  where
    iter :: Integer -> Integer -> Integer -> Integer
    iter m n acc
      | m < n     = acc
      | otherwise = iter (m - n) n (acc + 1)
