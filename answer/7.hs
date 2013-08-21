{-# LANGUAGE TemplateHaskell #-}

-- % cabal install memoize
-- % cabal install hspec
-- % runghc <this_file>

import Data.Function.Memoize
import Test.Hspec

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

data FunID = A | B | C | D | E | F deriving (Eq, Ord, Enum)
deriveMemoizable ''FunID

us_coin_memo :: Integer -> Integer
us_coin_memo = usCoinMemo E

usCoinMemo :: FunID -> Integer -> Integer
usCoinMemo = memoize2 usCoin

usCoin :: FunID -> Integer -> Integer
usCoin _ n | n < 0 = 0
usCoin A _         = 1
usCoin B n         = usCoinMemo A n + usCoinMemo B (n-5)
usCoin C n         = usCoinMemo B n + usCoinMemo C (n-10)
usCoin D n         = usCoinMemo C n + usCoinMemo D (n-25)
usCoin E n         = usCoinMemo D n + usCoinMemo E (n-50)
usCoin _ _         = error "usCoin"

ja_coin_memo :: Integer -> Integer
ja_coin_memo = jaCoinMemo E

jaCoinMemo :: FunID -> Integer -> Integer
jaCoinMemo = memoize2 jaCoin

jaCoin :: FunID -> Integer -> Integer
jaCoin _ n | n < 0 = 0
jaCoin A _         = 1
jaCoin B n         = jaCoinMemo A n + jaCoinMemo B (n-5)
jaCoin C n         = jaCoinMemo B n + jaCoinMemo C (n-10)
jaCoin D n         = jaCoinMemo C n + jaCoinMemo D (n-50)
jaCoin E n         = jaCoinMemo D n + jaCoinMemo E (n-100)
jaCoin F n         = jaCoinMemo E n + jaCoinMemo F (n-500)

----------------------------------------------------------------

my_fib_memo :: Integer -> Integer
my_fib_memo = memoize my_fib

my_fib :: Integer -> Integer
my_fib 0 = 0
my_fib 1 = 1
my_fib n = my_fib_memo (n - 2) + my_fib_memo (n - 1)

----------------------------------------------------------------

my_catalan_memo :: Integer -> Integer
my_catalan_memo n = my_cat_memo n n

my_cat_memo :: Integer -> Integer -> Integer
my_cat_memo = memoize2 my_cat

my_cat :: Integer -> Integer -> Integer
my_cat _ 0 = 1
my_cat m n
  | m == n    = my_cat_memo m (n - 1)
  | otherwise = my_cat_memo m (n - 1) + my_cat_memo (m - 1) n

----------------------------------------------------------------

my_catalan2_memo :: Integer -> Integer
my_catalan2_memo = memoize my_catalan2

my_catalan2 :: Integer -> Integer
my_catalan2 0 = 1
my_catalan2 n = sum (zipWith (*) xs ys)
  where
    xs = map my_catalan2_memo [0 .. n - 1]
    ys = map my_catalan2_memo [n - 1, n - 2 .. 0]

----------------------------------------------------------------

my_coin_memo :: [Integer] -> Integer -> Integer
my_coin_memo = memoize2 my_coin

my_coin :: [Integer] -> Integer -> Integer
my_coin _ 0  = 1
my_coin [] _ = 0
my_coin ccs@(c:cs) n
  | n < 0     = 0
  | otherwise = my_coin_memo cs n + my_coin_memo ccs (n - c)

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
            let xs = [1..150]
            map (my_coin_memo [1,5,10,25,50]) xs `shouldBe` map us_coin_memo xs
        it "calculates the same results of Japanese coins " $ do
            let xs = [1..150]
            map (my_coin_memo [1,5,10,50,100,500]) xs `shouldBe` map ja_coin_memo xs
