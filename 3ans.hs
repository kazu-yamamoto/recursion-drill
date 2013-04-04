-- % cabal install hspec
-- % runghc <this_file>

import Small

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
    describe "divide" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 1
        ==> divide m n == m `div` n
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
    describe "divide2" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 0 && n >= 1
        ==> divide2 m n == m `div` n

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

divide :: Int -> Int -> Int
divide m n
  | m < n     = 0
  | otherwise = divide (m - n) n + 1

gt :: Int -> Int -> Bool
gt 0 _ = False
gt _ 0 = True
gt m n = gt (m - 1) (n - 1)

gteq :: Int -> Int -> Bool
gteq _ 0 = True
gteq 0 _ = False
gteq m n = gteq (m - 1) (n - 1)

isOdd :: Int -> Bool
isOdd 0 = False
isOdd 1 = True
isOdd n = isOdd (n - 2)

remainder :: Int -> Int -> Int
remainder m n
  | m < n     = m
  | otherwise = remainder (m - n) n

divide2 :: Int -> Int -> Int
divide2 m n = divide2' m n 0

divide2' :: Int -> Int -> Int -> Int
divide2' m n acc
  | m < n     = acc
  | otherwise = divide2' (m - n) n (acc + 1)
