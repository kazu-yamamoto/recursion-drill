{-# LANGUAGE TemplateHaskell #-}

-- % cabal install memoize
-- % cabal install hspec
-- % runghc <this_file>

import Data.Function.Memoize
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "my_fib_memo" $
        it "calculates the same results of model" $ do
            let xs = [1..10]
            map my_fib_memo xs `shouldBe` map fibModel xs
    describe "my_catalan_memo" $
        it "calculates the same results of formula" $ do
            let xs = [1..10]
            map my_catalan_memo xs `shouldBe` map catalanFormula xs
    describe "my_catalan2_memo" $
        it "calculates the same results of formula" $ do
            let xs = [1..10]
            map my_catalan2_memo xs `shouldBe` map catalanFormula xs
    describe "my_coin_memo" $ do
        it "calculates the same results of America coins " $ do
            let xs = [1..150]
            map (flip my_coin_memo [1,5,10,25,50]) xs `shouldBe` map usCoin xs
        it "calculates the same results of Japanese coins " $ do
            let xs = [1..150]
            map (flip my_coin_memo [1,5,10,50,100,500]) xs `shouldBe` map jaCoin xs

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

usCoin :: Integer -> Integer
usCoin n = memoFix usCoinF (E,n)

usCoinF :: ((FunID, Integer) -> Integer) -> ((FunID, Integer) -> Integer)
usCoinF _ (_,n) | n < 0 = 0
usCoinF _ (A,_)         = 1
usCoinF f (B,n)         = f (A,n) + f (B,n-5)
usCoinF f (C,n)         = f (B,n) + f (C,n-10)
usCoinF f (D,n)         = f (C,n) + f (D,n-25)
usCoinF f (E,n)         = f (D,n) + f (E,n-50)
usCoinF _ _             = error "usCoinF"

jaCoin :: Integer -> Integer
jaCoin n = memoFix jaCoinF (E,n)

jaCoinF :: ((FunID, Integer) -> Integer) -> ((FunID, Integer) -> Integer)
jaCoinF _ (_,n) | n < 0 = 0
jaCoinF _ (A,_)         = 1
jaCoinF f (B,n)         = f (A,n) + f (B,n-5)
jaCoinF f (C,n)         = f (B,n) + f (C,n-10)
jaCoinF f (D,n)         = f (C,n) + f (D,n-50)
jaCoinF f (E,n)         = f (D,n) + f (E,n-100)
jaCoinF f (F,n)         = f (E,n) + f (F,n-500)

----------------------------------------------------------------

my_fib_memo :: Integer -> Integer
my_fib_memo x = memoFix fibF x
  where
    fibF :: (Integer -> Integer) -> (Integer -> Integer)
    fibF _ 0 = 0
    fibF _ 1 = 1
    fibF f n = f (n - 2) + f (n - 1)

----------------------------------------------------------------

my_catalan_memo :: Integer -> Integer
my_catalan_memo x = memoFix2 catF x x
  where
    catF :: (Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer)
    catF _ _ 0 = 1
    catF f m n
      | m == n    = f m (n - 1)
      | otherwise = f m (n - 1) + f (m - 1) n

----------------------------------------------------------------

my_catalan2_memo :: Integer -> Integer
my_catalan2_memo x = memoFix catF x
  where
    catF :: (Integer -> Integer) -> (Integer -> Integer)
    catF _ 0 = 1
    catF f n = sum (zipWith (*) xs ys)
      where
        xs = map f [0 .. n - 1]
        ys = map f [n - 1, n - 2 .. 0]

----------------------------------------------------------------

my_coin_memo :: Integer -> [Integer] -> Integer
my_coin_memo x xs = memoFix coinF (x,xs)
  where
    coinF :: ((Integer,[Integer]) -> Integer) -> ((Integer,[Integer]) -> Integer)
    coinF _ (0,_)  = 1
    coinF _ (_,[]) = 0
    coinF f (n,ccs@(c:cs))
      | n < 0     = 0
      | otherwise = f (n,cs) + f ((n - c),ccs)

----------------------------------------------------------------

data FunID = A | B | C | D | E | F deriving (Eq, Ord, Enum)
deriveMemoizable ''FunID
