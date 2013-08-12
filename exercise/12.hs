{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck

----------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "my_preorder" $
        prop "behaves like model" $ \(es :: [Int]) -> do
            let t = my_from_list es
            my_preorder t `shouldBe` my_preorder_slow t
    describe "my_inorder" $
        prop "behaves like model" $ \(es :: [Int]) -> do
            let t = my_from_list es
            my_inorder t `shouldBe` my_inorder_slow t
    describe "my_postorder" $
        prop "behaves like model" $ \(es :: [Int]) -> do
            let t = my_from_list es
            my_postorder t `shouldBe` my_postorder_slow t
    describe "my_delete" $ do
        prop "returns a right tree" $ \(es :: [Int]) -> do
            let t = my_from_list es
                e = if null es then 0 else head es
                t' = my_delete e t
                es' = delete e (nub es)
            prop_ordered t' es'
        prop "returns an empty tree if applied to all members" $ \(es :: [Int]) -> do
            let t = my_from_list es
                t' = foldl (flip my_delete) t es
            t' `shouldBe` Leaf

prop_ordered :: Tree Int -> [Int] -> Expectation
prop_ordered t es = my_inorder t `shouldBe` nub (sort es)

----------------------------------------------------------------

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq,Show)

----------------------------------------------------------------

my_insert :: Ord a => a -> Tree a -> Tree a
my_insert e Leaf = Node Leaf e Leaf
my_insert e (Node l x r) = case compare e x of
    LT -> Node (my_insert e l) x r
    EQ -> Node l e r
    GT -> Node l x (my_insert e r)

----------------------------------------------------------------

my_from_list :: Ord a => [a] -> Tree a
my_from_list es = foldl ins Leaf es
  where
    ins t e = my_insert e t

----------------------------------------------------------------

my_preorder_slow :: Tree a -> [a]
my_preorder_slow Leaf         = []
my_preorder_slow (Node l x r) =
    [x] ++ my_preorder_slow l ++ my_preorder_slow r

my_inorder_slow :: Tree a -> [a]
my_inorder_slow Leaf         = []
my_inorder_slow (Node l x r) =
    my_inorder_slow l ++ [x] ++ my_inorder_slow r

my_postorder_slow :: Tree a -> [a]
my_postorder_slow Leaf         = []
my_postorder_slow (Node l x r) =
    my_postorder_slow l ++ my_postorder_slow r ++ [x]

----------------------------------------------------------------

my_preorder :: Tree a -> [a]
my_preorder t = my_preorder' t []

my_preorder' :: Tree a -> [a] -> [a]
my_preorder' Leaf         es = es
my_preorder' (Node l x r) es = undefined

my_inorder :: Tree a -> [a]
my_inorder t = my_inorder' t []

my_inorder' :: Tree a -> [a] -> [a]
my_inorder' Leaf         es = es
my_inorder' (Node l x r) es = undefined

my_postorder :: Tree a -> [a]
my_postorder t = my_postorder' t []

my_postorder' :: Tree a -> [a] -> [a]
my_postorder' Leaf         es = es
my_postorder' (Node l x r) es = undefined

----------------------------------------------------------------

my_delete_min :: Ord a => Tree a -> (a, Tree a)
my_delete_min Leaf            = error "my_delete_min"
my_delete_min (Node Leaf x r) = (x, r)
my_delete_min (Node l x r)    = undefined

----------------------------------------------------------------

my_delete :: Ord a => a -> Tree a -> Tree a
my_delete _ Leaf = Leaf
my_delete e (Node l x r) = case compare e x of
    LT -> undefined
    EQ -> my_glue l r
    GT -> undefined

my_glue :: Ord a => Tree a -> Tree a -> Tree a
my_glue Leaf r = undefined
my_glue l Leaf = undefined
my_glue l r    = undefined
