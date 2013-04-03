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
    describe "lt" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> lt m n == (m < n)
    describe "lteq" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> lteq m n == (m <= n)
    describe "isEven" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> isEven n == even n
    describe "devide" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 1
        ==> devide m n == m `div` n
    describe "gt" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> gt m n == (m > n)
    describe "gteq" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 0
        ==> gteq m n == (m >= n)
    describe "isOdd" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> isOdd n == odd n
    describe "remainder" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 1
        ==> remainder m n == m `mod` n
    describe "devide2" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 1
        ==> devide2 m n == m `div` n

lt :: Int -> Int -> Bool
lt _ 0 = False
lt 0 _ = True
lt m n = lt (m - 1) (n - 1)

lteq :: Int -> Int -> Bool
lteq 0 _ = True
lteq _ 0 = False
lteq m n = lteq (m - 1) (n - 1)

isEven :: Int -> Bool
isEven 0 = True
isEven 1 = False
isEven n = isEven (n - 2)

devide :: Int -> Int -> Int
devide n m
  | n < m     = 0
  | otherwise = devide (n - m) m + 1

gt :: Int -> Int -> Bool
gt = undefined

gteq :: Int -> Int -> Bool
gteq = undefined

isOdd :: Int -> Bool
isOdd = undefined

remainder :: Int -> Int -> Int
remainder = undefined

devide2 :: Int -> Int -> Int
devide2 = undefined
