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
        ==> gcdFast m n == gcd m n
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
gcdFast = undefined

lcmFast :: Int -> Int -> Int
lcmFast = undefined

----------------------------------------------------------------

power :: Int -> Int -> Int
power _ 0 = 1
power m n = power m (n - 1) * m

powerFast :: Int -> Int -> Int
powerFast _ 0 = 1
powerFast m n
  | odd n     = powerFast undefined undefined * m
  | otherwise = powerFast undefined undefined

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
  | odd n     = powerFastIter' undefined undefined undefined
  | otherwise = powerFastIter' undefined undefined undefined

----------------------------------------------------------------

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibFast :: Int -> Integer
fibFast n = fibFast' n 0 1

fibFast' :: Int -> Integer -> Integer -> Integer
fibFast' 0 x _ = x
fibFast' n x y = fibFast' undefined undefined undefined

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
