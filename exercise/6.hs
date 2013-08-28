-- % cabal install hspec
-- % runghc <this_file>

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "my_catalan" $
        it "calculates the same results of formula" $ do
            let xs = [1..10]
            map my_catalan xs `shouldBe` map catalanFormula xs
    describe "my_catalan2" $
        it "calculates the same results of formula" $ do
            let xs = [1..10]
            map my_catalan2 xs `shouldBe` map catalanFormula xs
    describe "my_coin" $ do
        it "calculates the same results of America coins " $ do
            let xs = [1..150]
            map (`my_coin` [1,5,10,25,50]) xs `shouldBe` map usCoin xs
        it "calculates the same results of Japanese coins " $ do
            let xs = [1..150]
            map (`my_coin` [1,5,10,50,100,500]) xs `shouldBe` map jaCoin xs

----------------------------------------------------------------

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

my_catalan :: Integer -> Integer
my_catalan x = my_cat x x

my_cat :: Integer -> Integer -> Integer
my_cat _ 0 = 1
my_cat m n
  | m == n    = undefined
  | otherwise = undefined

----------------------------------------------------------------

my_catalan2 :: Integer -> Integer
my_catalan2 0 = 1
my_catalan2 n = sum (zipWith (*) xs ys)
  where
    xs = undefined
    ys = undefined

----------------------------------------------------------------

my_coin :: Integer -> [Integer] -> Integer
my_coin 0 _   = 1
my_coin _ []  = 0
my_coin n (c:cs)
  | n < 0     = 0
  | otherwise = undefined
