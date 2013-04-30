-- % cabal install hspec
-- % runghc <this_file>

import Small

main :: IO ()
main = hspec $ do
    describe "my_soap" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> my_soap n == sum [0..n]
    describe "my_fact" $
      prop "behaves as model" $ \(Small n) -> n >= 1
        ==> my_fact n == product [1..n]
    describe "my_mul" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 1
        ==> my_mul m n == m * n
    describe "my_plus" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> my_plus m n == m + n
    describe "my_minus" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> my_minus m n == m - n
    describe "my_power" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 0
        ==> my_power m n == m ^ n

my_soap :: Integer -> Integer
my_soap 0 = 0
my_soap n = my_soap (n-1) + n

my_fact :: Integer -> Integer
my_fact 1 = 1
my_fact n = my_fact (n - 1) * n

my_mul :: Integer -> Integer -> Integer
my_mul m 1 = m
my_mul m n = my_mul m (n - 1) + m

my_plus :: Integer -> Integer -> Integer
my_plus = undefined

my_minus :: Integer -> Integer -> Integer
my_minus = undefined

my_power :: Integer -> Integer -> Integer
my_power = undefined
