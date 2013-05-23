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

usCoinA,usCoinB,usCoinC,usCoinD,usCoin :: Integer -> Integer
usCoinA n | n < 0 = 0
usCoinA _         = 1
usCoinB n | n < 0 = 0
usCoinB n         = usCoinA n + usCoinB (n - 5)
usCoinC n | n < 0 = 0
usCoinC n         = usCoinB n + usCoinC (n - 10)
usCoinD n | n < 0 = 0
usCoinD n         = usCoinC n + usCoinD (n - 25)
usCoin  n | n < 0 = 0
usCoin  n         = usCoinD n + usCoin  (n - 50)

jaCoinA,jaCoinB,jaCoinC,jaCoinD,jaCoinE,jaCoin :: Integer -> Integer
jaCoinA n | n < 0 = 0
jaCoinA _         = 1
jaCoinB n | n < 0 = 0
jaCoinB n         = jaCoinA n + jaCoinB (n - 5)
jaCoinC n | n < 0 = 0
jaCoinC n         = jaCoinB n + jaCoinC (n - 10)
jaCoinD n | n < 0 = 0
jaCoinD n         = jaCoinC n + jaCoinD (n - 50)
jaCoinE n | n < 0 = 0
jaCoinE n         = jaCoinD n + jaCoinE (n - 100)
jaCoin  n | n < 0 = 0
jaCoin  n         = jaCoinE n + jaCoin  (n - 500)

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
my_catalan_memo x = undefined
  where
    catF :: (Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer)
    catF = undefined

----------------------------------------------------------------

my_catalan2_memo :: Integer -> Integer
my_catalan2_memo x = undefined
  where
    catF :: (Integer -> Integer) -> (Integer -> Integer)
    catF = undefined

----------------------------------------------------------------

my_coin_memo :: Integer -> [Integer] -> Integer
my_coin_memo x = undefined
  where
    coinF :: (Integer -> [Integer] -> Integer) -> (Integer -> [Integer] -> Integer)
    coinF = undefined
