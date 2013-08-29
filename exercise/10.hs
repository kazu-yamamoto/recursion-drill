module Main where

import Data.Char
import Test.Hspec

----------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "my_paren_seq" $
        it "counts parenthesis sequentially" $ do
            my_paren_seq ""       `shouldBe` (0,"")
            my_paren_seq "()a"    `shouldBe` (1,"a")
            my_paren_seq "()()()" `shouldBe` (3,"")
    describe "my_paren_req" $
        it "counts parenthesis recursively" $ do
            my_paren_rec ""       `shouldBe` (0,"")
            my_paren_rec "()a"    `shouldBe` (1,"a")
            my_paren_rec "((()))" `shouldBe` (3,"")
    describe "my_paren" $
        it "counts parenthesis" $ do
            my_paren ""       `shouldBe` (0,"")
            my_paren "()a"    `shouldBe` (1,"a")
            my_paren "()()()" `shouldBe` (3,"")
            my_paren "((()))" `shouldBe` (3,"")
            my_paren "((()()))()(()()())" `shouldBe` (9,"")
    describe "my_expr_simple" $
        it "calculates a simple mathematical expression" $ do
            my_expr_simple "1"      `shouldBe` (1,"")
            my_expr_simple "1+2"    `shouldBe` (3,"")
            my_expr_simple "1+2+3a" `shouldBe` (6,"a")
    describe "my_expr" $
        it "calculates a simple mathematical expression" $ do
            my_expr "1"             `shouldBe` (1,"")
            my_expr "1+2"           `shouldBe` (3,"")
            my_expr "1+2+3a"        `shouldBe` (6,"a")
            my_expr "(1+2)+3"       `shouldBe` (6,"")
            my_expr "(1+2)*3"       `shouldBe` (9,"")
            my_expr "(1+2)*(3+4)+5" `shouldBe` (26,"")

----------------------------------------------------------------
----------------------------------------------------------------

my_close :: String -> (Int, String)
my_close (')':left) = (1,left)
my_close _          = error "no my_close parenthesis"

----------------------------------------------------------------
-- Counting parenthesis sequentially

my_paren_seq :: String -> (Int, String)
my_paren_seq ('(':left0) = my_open_seq left0
my_paren_seq left0       = (0,left0)

my_open_seq :: String -> (Int, String)
my_open_seq left0 = (cnt1+cnt2, left2)
  where
    (cnt1,left1) = my_close     left0
    (cnt2,left2) = my_paren_seq left1

----------------------------------------------------------------
-- Counting parenthesis recursively

my_paren_rec :: String -> (Int, String)
my_paren_rec ('(':left0) = my_open_rec left0
my_paren_rec left0       = (0,left0)

my_open_rec :: String -> (Int, String)
my_open_rec left0 = (cnt1+cnt2, left2)
  where
    (cnt1,left1) = my_paren_rec left0
    (cnt2,left2) = my_close     left1

----------------------------------------------------------------
-- Counting parenthesis generally

my_paren :: String -> (Int, String)
my_paren ('(':left0) = my_open left0
my_paren left0       = (0,left0)

my_open :: String -> (Int, String)
my_open left0 = undefined

----------------------------------------------------------------
----------------------------------------------------------------
-- Parser for simple mathematical expressions

{-
<expr_simple> ::= <nat> (’+’ <expr_simple> | e)
<nat>         ::= ’0’ | ’1’ | ’2’ | ...
-}

my_nat :: String -> (Int, String)
my_nat (x:xs)
  | isDigit x = (c2i x, xs)
my_nat _      = error "my_nat"

c2i :: Char -> Int
c2i x = ord x - ord '0'

my_first :: (a -> c) -> (a,b) -> (c,b)
my_first f (x,y) = (f x, y)

my_expr_simple :: String -> (Int, String)
my_expr_simple xs = case my_nat xs of
    (x, '+':left) -> my_first (x+) (my_expr_simple left)
    xl            -> xl

----------------------------------------------------------------
-- Parser for simple mathematical expressions

{-
<expr>   ::= <term>   (’+’ <expr> | e)
<term>   ::= <factor> (’*’ <term> | e)
<factor> ::= ’(’ <expr> ’)’ | <nat>
<nat>    ::=’0’ | ’1’ | ’2’ | ...
-}

my_expr :: String -> (Int, String)
my_expr xs = undefined

my_term :: String -> (Int, String)
my_term xs = undefined

my_factor :: String -> (Int, String)
my_factor xs = undefined
