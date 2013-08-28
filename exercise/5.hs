-- % cabal install hspec
-- % runghc <this_file>

import Small

main :: IO ()
main = hspec $ do
    describe "my_gcd" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 2 && n >= 2
        ==> if m >= n then
                my_gcd m n == gcd m n
            else
                my_gcd n m == gcd n m
    describe "my_gcd_fast" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 2 && n >= 2
        ==> my_gcd_fast m n == gcd m n
    describe "my_lcm_fast" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 2 && n >= 2
        ==> my_lcm_fast m n == lcm m n
    describe "my_power" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 0
        ==> my_power m n == m ^ n
    describe "my_power_iter" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 0
        ==> my_power_iter m n == m ^ n
    describe "my_power_fast" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 0
        ==> my_power_fast m n == m ^ n
    describe "my_power_fast_iter" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 0
        ==> my_power_fast_iter m n == m ^ n
    describe "my_fib" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> my_fib n == fibModel n
    describe "my_fib_iter" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> my_fib_iter n == fibModel n
    describe "my_even_m" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> my_even_m n == even n
    describe "my_odd_m" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> my_odd_m n == odd n
    describe "my_even_m2" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> my_even_m2 n == even n
    describe "my_odd_m2" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> my_odd_m2 n == odd n

----------------------------------------------------------------

my_gcd :: Integer -> Integer -> Integer
my_gcd a 0 = a
my_gcd a b
  | c >= b    = my_gcd c b
  | otherwise = my_gcd b c
  where
    c = a - b

my_gcd_fast :: Integer -> Integer -> Integer
my_gcd_fast = undefined

my_lcm_fast :: Integer -> Integer -> Integer
my_lcm_fast = undefined

----------------------------------------------------------------

my_power :: Integer -> Integer -> Integer
my_power _ 0 = 1
my_power m n = my_power m (n - 1) * m

my_power_fast :: Integer -> Integer -> Integer
my_power_fast _ 0 = 1
my_power_fast m n
  | odd n     = my_power_fast undefined undefined * m
  | otherwise = my_power_fast undefined undefined

my_power_iter :: Integer -> Integer -> Integer
my_power_iter x y = iter x y 1
  where
    iter :: Integer -> Integer -> Integer -> Integer
    iter _ 0 acc = acc
    iter m n acc = iter m (n - 1) (acc * m)

my_power_fast_iter :: Integer -> Integer -> Integer
my_power_fast_iter x y = iter x y 1
  where
    iter :: Integer -> Integer -> Integer -> Integer
    iter _ 0 acc = acc
    iter m n acc
      | odd n     = iter undefined undefined undefined
      | otherwise = iter undefined undefined undefined

----------------------------------------------------------------

my_fib :: Integer -> Integer
my_fib 0 = 0
my_fib 1 = 1
my_fib n = my_fib (n - 2) + my_fib (n - 1)

my_fib_iter :: Integer -> Integer
my_fib_iter a = iter a 0 1
  where
    iter :: Integer -> Integer -> Integer -> Integer
    iter 0 x _ = x
    iter n x y = iter undefined undefined undefined

fibModel :: Integer -> Integer
fibModel n = fibs !! fromInteger n

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

----------------------------------------------------------------

my_even_m :: Integer -> Bool
my_even_m 0 = True
my_even_m n = my_odd_m (n - 1)

my_odd_m :: Integer -> Bool
my_odd_m 0 = False
my_odd_m n = my_even_m (n - 1)

----------------------------------------------------------------

my_even_m2 :: Integer -> Bool
my_even_m2 0 = True
my_even_m2 n = undefined

my_odd_m2 :: Integer -> Bool
my_odd_m2 1 = True
my_odd_m2 n = undefined
