-- % cabal install hspec
-- % runghc <this_file>

import Small

main :: IO ()
main = hspec $ do
    describe "my_soap_iter" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> my_soap_iter n == sum [0..n]
    describe "my_fact_iter" $
      prop "behaves as model" $ \(Small n) -> n >= 1
        ==> my_fact_iter n == product [1..n]
    describe "my_mul_iter" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 1
        ==> my_mul_iter m n == m * n
    describe "my_plus_iter" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> my_plus_iter m n == m + n
    describe "my_minus_iter" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> my_minus_iter m n == m - n
    describe "my_power_iter" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 0
        ==> my_power_iter m n == m ^ n

-- my_soap :: Integer -> Integer
-- my_soap 0 = 0
-- my_soap n = soap (n-1) + n

my_soap_iter :: Integer -> Integer
my_soap_iter x = iter x 0
  where
    iter :: Integer -> Integer -> Integer
    iter 0 acc = acc
    iter n acc = iter (n-1) (acc + n)

-- my_fact :: Integer -> Integer
-- my_fact 1 = 1
-- my_fact n = fact (n - 1) * n

my_fact_iter :: Integer -> Integer
my_fact_iter x = iter x 1
  where
    iter :: Integer -> Integer -> Integer
    iter 1 acc = acc
    iter n acc = iter (n - 1) (acc * n)

-- my_mul :: Integer -> Integer -> Integer
-- my_mul m 1 = m
-- my_mul m n = mul m (n - 1) + m

my_mul_iter :: Integer -> Integer -> Integer
my_mul_iter x y = iter x y x
  where
    iter :: Integer -> Integer -> Integer -> Integer
    iter _ 1 acc = acc
    iter m n acc = iter m (n - 1) (acc + m)

-- my_plus :: Integer -> Integer -> Integer
-- my_plus m 0 = m
-- my_plus m n = plus m (n - 1) + 1

my_plus_iter :: Integer -> Integer -> Integer
my_plus_iter x y = iter x y undefined
  where
    iter :: Integer -> Integer -> Integer -> Integer
    iter = undefined

-- my_minus :: Integer -> Integer -> Integer
-- my_minus m 0 = m
-- my_minus m n = minus m (n - 1) - 1

my_minus_iter :: Integer -> Integer -> Integer
my_minus_iter x y = iter x y undefined
  where
    iter :: Integer -> Integer -> Integer -> Integer
    iter = undefined

-- my_power :: Integer -> Integer -> Integer
-- my_power _ 0 = 1
-- my_power m n = power m (n - 1) * m

my_power_iter :: Integer -> Integer -> Integer
my_power_iter x y = iter x y undefined
  where
    iter :: Integer -> Integer -> Integer -> Integer
    iter = undefined
