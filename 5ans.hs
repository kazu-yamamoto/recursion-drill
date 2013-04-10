-- % cabal install hspec
-- % runghc <this_file>

import Small

main :: IO ()
main = hspec $ do
    describe "gcdSlow" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 2 && n >= 2
        ==> if m >= n then 
                gcdSlow m n == gcd m n
            else
                gcdSlow n m == gcd n m
    describe "gcdFast" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 2 && n >= 2
        ==> if m >= n then 
                gcdFast m n == gcd m n
            else
                gcdFast n m == gcd n m
    describe "lcmFast" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 2 && n >= 2
        ==> lcmFast m n == lcm m n
    describe "power" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 0
        ==> power m n == m ^ n
    describe "powerIter" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 0
        ==> powerIter m n == m ^ n
    describe "powerFast" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 0
        ==> powerFast m n == m ^ n
    describe "powerFastIter" $
      prop "behaves as model" $ \(Small m) (Small n) -> m >= 1 && n >= 0
        ==> powerFastIter m n == m ^ n
    describe "fibFast" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> fibFast n == fibModel n
    describe "isEven" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> isEven n == even n
    describe "isOdd" $
      prop "behaves as model" $ \(Small n) -> n >= 0
        ==> isOdd n == odd n

----------------------------------------------------------------

gcdSlow :: Int -> Int -> Int
gcdSlow a 0 = a
gcdSlow a b
  | c >= b    = gcdSlow c b
  | otherwise = gcdSlow b c
  where
    c = a - b

gcdFast :: Int -> Int -> Int
gcdFast a 0 = a
gcdFast a b = gcdFast b (a `mod` b)

lcmFast :: Int -> Int -> Int
lcmFast a b
  | a >= b    = a * b `div` gcd a b
  | otherwise = b * a `div` gcd b a

----------------------------------------------------------------

power :: Int -> Int -> Int
power _ 0 = 1
power m n = power m (n - 1) * m

powerFast :: Int -> Int -> Int
powerFast _ 0 = 1
powerFast m n
  | odd n     = powerFast (m*m) (n `div` 2) * m
  | otherwise = powerFast (m*m) (n `div` 2)

{-
powerFast :: Int -> Int -> Int
powerFast _ 0 = 1
powerFast m n
  | even n    = powerFast (m*m) (n `div` 2)
  | otherwise = powerFast m (n - 1) * m
-}

powerIter :: Int -> Int -> Int
powerIter m n = powerIter' m n 1

powerIter' :: Int -> Int -> Int -> Int
powerIter' _ 0 acc = acc
powerIter' m n acc = powerIter' m (n - 1) (acc * m)

powerFastIter :: Int -> Int -> Int
powerFastIter m n = powerFastIter' m n 1

powerFastIter' :: Int -> Int -> Int -> Int
powerFastIter' _ 0 acc = acc
powerFastIter' m n acc
  | odd n     = powerFastIter' (m*m) (n `div` 2) (acc * m)
  | otherwise = powerFastIter' (m*m) (n `div` 2) acc

----------------------------------------------------------------

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibFast :: Int -> Integer
fibFast n = fibFast' n 0 1

fibFast' :: Int -> Integer -> Integer -> Integer
fibFast' 0 x _ = x
fibFast' n x y = fibFast' (n - 1) y (x + y)

fibModel :: Int -> Integer
fibModel n = fibs !! n
    
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

----------------------------------------------------------------

isEven :: Int -> Bool
isEven 0 = True
isEven n = isOdd (n - 1)

isOdd :: Int -> Bool
isOdd 0 = False
isOdd n = isEven (n - 1)
