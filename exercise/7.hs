-- % cabal install memoize
-- % cabal install hspec
-- % runghc <this_file>

import Data.Function.Memoize
import Test.Hspec

----------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "my_fib_memo" $
        it "calculates the same results of model" $ do
            let xs = [1..100]
            map my_fib_memo xs `shouldBe` map fibModel xs
    describe "my_catalan_memo" $
        it "calculates the same results of formula" $ do
            let xs = [1..100]
            map my_catalan_memo xs `shouldBe` map catalanFormula xs
    describe "my_catalan2_memo" $
        it "calculates the same results of formula" $ do
            let xs = [1..100]
            map my_catalan2_memo xs `shouldBe` map catalanFormula xs
    describe "my_coin_memo" $ do
        it "calculates the same results of America coins " $ do
            let xs = [1..10000]
            map (`my_coin_memo` [1,5,10,25,50]) xs `shouldBe` map usCoinMemo xs
        it "calculates the same results of Japanese coins " $ do
            let xs = [1..10000]
            map (`my_coin_memo` [1,5,10,50,100,500]) xs `shouldBe` map jaCoinMemo xs
----------------------------------------------------------------

fibModel :: Integer -> Integer
fibModel n = fibs !! fromInteger n

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

catalanFormula :: Integer -> Integer
catalanFormula n = bang (2 * n) `div` bang (n + 1) `div` bang n
  where
    bang x = product [1..x]

----------------------------------------------------------------

usCoinMemo :: Integer -> Integer
usCoinMemo = usCoinMemoE

usCoinMemoA :: Integer -> Integer
usCoinMemoA _ = 1
usCoinMemoB :: Integer -> Integer
usCoinMemoB = memoize g
  where
    g n | n < 0     = 0
        | otherwise = usCoinMemoA n + usCoinMemoB (n-5)
usCoinMemoC :: Integer -> Integer
usCoinMemoC  = memoize g
  where
    g n | n < 0     = 0
        | otherwise = usCoinMemoB n + usCoinMemoC (n-10)
usCoinMemoD :: Integer -> Integer
usCoinMemoD  = memoize g
  where
    g n | n < 0     = 0
        | otherwise = usCoinMemoC n + usCoinMemoD (n-25)
usCoinMemoE :: Integer -> Integer
usCoinMemoE  = memoize g
  where
    g n | n < 0     = 0
        | otherwise = usCoinMemoD n + usCoinMemoE (n-50)

----------------------------------------------------------------

jaCoinMemo :: Integer -> Integer
jaCoinMemo = jaCoinMemoF

jaCoinMemoA :: Integer -> Integer
jaCoinMemoA _ = 1
jaCoinMemoB :: Integer -> Integer
jaCoinMemoB = memoize g
  where
    g n | n < 0     = 0
        | otherwise = jaCoinMemoA n + jaCoinMemoB (n-5)
jaCoinMemoC :: Integer -> Integer
jaCoinMemoC  = memoize g
  where
    g n | n < 0     = 0
        | otherwise = jaCoinMemoB n + jaCoinMemoC (n-10)
jaCoinMemoD :: Integer -> Integer
jaCoinMemoD  = memoize g
  where
    g n | n < 0     = 0
        | otherwise = jaCoinMemoC n + jaCoinMemoD (n-50)
jaCoinMemoE :: Integer -> Integer
jaCoinMemoE  = memoize g
  where
    g n | n < 0     = 0
        | otherwise = jaCoinMemoD n + jaCoinMemoE (n-100)

jaCoinMemoF :: Integer -> Integer
jaCoinMemoF  = memoize g
  where
    g n | n < 0     = 0
        | otherwise = jaCoinMemoE n + jaCoinMemoF (n-500)

----------------------------------------------------------------

my_fib_memo :: Integer -> Integer
my_fib_memo = memoize my_fib

my_fib :: Integer -> Integer
my_fib 0 = 0
my_fib 1 = 1
my_fib n = my_fib_memo (n - 2) + my_fib_memo (n - 1)

----------------------------------------------------------------

{-
my_catalan :: Integer -> Integer
my_catalan x = my_cat x x

my_cat :: Integer -> Integer -> Integer
my_cat _ 0 = 1
my_cat m n
  | m == n    = my_cat m (n - 1)
  | otherwise = my_cat m (n - 1) + my_cat (m - 1) n
-}

my_catalan_memo :: Integer -> Integer
my_catalan_memo n = my_cat_memo n n

my_cat_memo :: Integer -> Integer -> Integer
my_cat_memo = undefined

my_cat :: Integer -> Integer -> Integer
my_cat = undefined

----------------------------------------------------------------

{-
my_catalan2 :: Integer -> Integer
my_catalan2 0 = 1
my_catalan2 n = sum (zipWith (*) xs ys)
  where
    xs = map my_catalan2 [0 .. n - 1]
    ys = map my_catalan2 [n - 1, n - 2 .. 0]
-}

my_catalan2_memo :: Integer -> Integer
my_catalan2_memo = undefined

my_catalan2 :: Integer -> Integer
my_catalan2 = undefined

----------------------------------------------------------------

{-
my_coin :: Integer -> [Integer] -> Integer
my_coin 0 _   = 1
my_coin _ []  = 0
my_coin n (c:cs)
  | n < 0     = 0
  | otherwise = my_coin n cs + my_coin (n - c) (c:cs)
-}

my_coin_memo :: Integer -> [Integer] -> Integer
my_coin_memo = undefined

my_coin :: Integer -> [Integer] -> Integer
my_coin = undefined
