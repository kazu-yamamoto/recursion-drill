{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
    describe "my_reverse" $
        prop "behaves like model" $ \(xs :: [Char]) ->
           my_reverse xs `shouldBe` reverse xs
    describe "my_reverse_iter" $
        prop "behaves like model" $ \(xs :: [Char]) ->
           my_reverse_iter xs `shouldBe` reverse xs
    describe "my_map" $
        prop "behaves like model" $ \(xs :: [Integer]) ->
           my_map (+1) xs `shouldBe` map (+1) xs
    describe "my_map_iter" $
        prop "behaves like model" $ \(xs :: [Integer]) ->
           my_map_iter (+1) xs `shouldBe` map (+1) xs
    describe "my_filter" $
        prop "behaves like model" $ \(xs :: [Integer]) -> do
           my_filter even xs `shouldBe` filter even xs
           my_filter odd  xs `shouldBe` filter odd  xs
           my_filter (>5) xs `shouldBe` filter (>5) xs
    describe "my_append" $
        prop "behaves like model" $ \(xs :: [Char]) (ys :: [Char]) ->
           xs `my_append` ys `shouldBe` xs ++ ys
    describe "my_concat" $
        prop "behaves like model" $ \(xxs :: [[Char]]) ->
           my_concat xxs `shouldBe` concat xxs
    describe "my_break" $
        prop "behaves like model" $ \(xs :: [Integer]) -> do
           my_break even xs `shouldBe` break even xs
           my_break odd  xs `shouldBe` break odd  xs
           my_break (>5) xs `shouldBe` break (>5) xs
    describe "my_intersperse" $
        prop "behaves like model" $ \(xs :: [Integer]) ->
           my_intersperse 1 xs `shouldBe` intersperse 1 xs
    describe "my_group" $
        prop "behaves like model" $ \(xs :: [Bool]) ->
           my_group xs `shouldBe` group xs

----------------------------------------------------------------

my_reverse :: [a] -> [a]
my_reverse []     = []
my_reverse (x:xs) = my_reverse xs ++ [x]

my_reverse_iter :: [a] -> [a]
my_reverse_iter as = iter as []
  where
    iter :: [a] -> [a] -> [a]
    iter []     ys = ys
    iter (x:xs) ys = iter xs (x:ys)

----------------------------------------------------------------

my_map :: (a -> b) -> [a] -> [b]
my_map _ []     = []
my_map f (x:xs) = f x : my_map f xs

my_map_iter :: (a -> b) -> [a] -> [b]
my_map_iter g as = reverse (iter g as [])
  where
    iter _ []     acc = acc
    iter f (x:xs) acc = iter f xs (f x : acc)

----------------------------------------------------------------

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter _ []     = []
my_filter p (x:xs) = undefined

----------------------------------------------------------------

my_append :: [a] -> [a] -> [a]
my_append []     ys = ys
my_append (x:xs) ys = undefined

my_concat :: [[a]] -> [a]
my_concat []       = []
my_concat (xs:xss) = undefined

----------------------------------------------------------------

my_intersperse :: a -> [a] -> [a]
my_intersperse = undefined

----------------------------------------------------------------

my_break :: (a -> Bool) -> [a] -> ([a], [a])
my_break _ [] = ([],[])
my_break p (x:xs) = undefined

----------------------------------------------------------------

my_group :: Eq a => [a] -> [[a]]
my_group []     = []
my_group (x:xs) = undefined
