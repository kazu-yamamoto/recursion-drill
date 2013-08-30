{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck

----------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "my_from_list" $
        prop "contains elements in order" $ \(es :: [Int]) ->
            prop_ordered (my_from_list es) es
    describe "my_member" $ do
        prop "behaves like model" prop_member_model
        prop "returns True for a memeber" prop_member

prop_ordered :: Tree Int -> [Int] -> Expectation
prop_ordered t es = inorder t `shouldBe` nub (sort es)

inorder :: Tree a -> [a]
inorder Leaf         = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

prop_member :: [Int] -> Expectation
prop_member [] = return ()
prop_member es = and rs `shouldBe` True
  where
    t = from_list es
    rs = [my_member e t | e <- es]

prop_member_model :: Int -> [Int] -> Expectation
prop_member_model x [] = my_member x Leaf `shouldBe` False
prop_member_model x es = my_member x t `shouldBe` elem x es
  where
    t = from_list es

-- This code is intentionally duplicated for the test of my_member.

from_list :: Ord a => [a] -> Tree a
from_list es = foldl ins Leaf es
  where
    ins :: Ord a => Tree a -> a -> Tree a
    ins Leaf e = Node Leaf e Leaf
    ins (Node l x r) e = case compare e x of
        LT -> Node (ins l e) x r
        EQ -> Node l e r
        GT -> Node l x (ins r e)

----------------------------------------------------------------

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq,Show)

----------------------------------------------------------------

my_member :: Ord a => a -> Tree a -> Bool
my_member _ Leaf = False
my_member e (Node l x r) = case compare e x of
    LT -> undefined
    EQ -> undefined
    GT -> undefined

----------------------------------------------------------------

my_insert :: Ord a => a -> Tree a -> Tree a
my_insert e Leaf = Node Leaf e Leaf
my_insert e (Node l x r) = case compare e x of
    LT -> undefined
    EQ -> Node l e r
    GT -> undefined

----------------------------------------------------------------

my_from_list :: Ord a => [a] -> Tree a
my_from_list es = foldl ins Leaf es
  where
    ins t e = my_insert e t

----------------------------------------------------------------

my_show_tree :: Show a => Tree a -> String
my_show_tree t = my_show_tree' t ""

my_show_tree' :: Show a => Tree a -> String -> String
my_show_tree' Leaf _               = ""
my_show_tree' (Node Leaf x Leaf) _ = show x
my_show_tree' (Node l x r) pref    =
    show x ++ "\n"
 ++ pref ++ "+" ++ my_show_tree' l (pref ++ " ") ++ "\n"
 ++ pref ++ "+" ++ my_show_tree' r (pref ++ " ")
