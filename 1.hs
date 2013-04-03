-- % cabal install hspec
-- % runghc 1.hs

import Control.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype Small = Small Int deriving Show
instance Arbitrary Small where
    arbitrary = Small . (`mod` 10) <$> arbitrary

main :: IO ()
main = hspec $ do
    describe "soap" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> soap n == sum [0..n]
    describe "fact" $
      prop "behaves as model" $ \(Small n) -> n >= 1
        ==> fact n == product [1..n]
    describe "mul" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 1
        ==> mul m n == m * n
    describe "plus" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> plus m n == m + n
    describe "minus" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> minus m n == m - n
    describe "power" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 1
        ==> power m n == m ^ n

soap :: Int -> Int
soap 0 = 0
soap n = soap (n-1) + n

fact :: Int -> Int
fact 1 = 1
fact n = fact (n - 1) * n

mul :: Int -> Int -> Int
mul m 1 = m
mul m n = mul m (n - 1) + m

plus :: Int -> Int -> Int
plus = undefined

minus :: Int -> Int -> Int
minus = undefined

power :: Int -> Int -> Int
power = undefined
