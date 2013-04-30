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
my_gt = undefined

my_gteq :: Integer -> Integer -> Bool
my_gteq = undefined

my_odd :: Integer -> Bool
my_odd = undefined

my_remainder :: Integer -> Integer -> Integer
my_remainder = undefined

my_divide_iter :: Integer -> Integer -> Integer
my_divide_iter x y = iter x y undefined
  where
    iter :: Integer -> Integer -> Integer -> Integer
    iter = undefined
