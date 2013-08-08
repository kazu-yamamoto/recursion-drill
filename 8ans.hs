{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "my_length" $
        prop "behaves like model" $ \(xs :: [Integer]) ->
           my_length xs `shouldBe` fromIntegral (length xs)
    describe "my_length_iter" $
        prop "behaves like model" $ \(xs :: [Integer]) ->
           my_length_iter xs `shouldBe` fromIntegral (length xs)
    describe "my_sum" $
        prop "behaves like model" $ \(xs :: [Integer]) ->
           my_sum xs `shouldBe` fromIntegral (sum xs)
    describe "my_sum_iter" $
        prop "behaves like model" $ \(xs :: [Integer]) ->
           my_sum_iter xs `shouldBe` fromIntegral (sum xs)
    describe "my_product" $
        prop "behaves like model" $ \(xs :: [Integer]) ->
           my_product xs `shouldBe` fromIntegral (product xs)
    describe "my_product_iter" $
        prop "behaves like model" $ \(xs :: [Integer]) ->
           my_product_iter xs `shouldBe` fromIntegral (product xs)
    describe "my_maximum" $
        prop "behaves like model" $ \(xs :: [Integer]) -> xs /= []
          ==> my_maximum xs `shouldBe` maximum xs
    describe "my_maximum_iter" $
        prop "behaves like model" $ \(xs :: [Integer]) -> xs /= []
          ==> my_maximum_iter xs `shouldBe` maximum xs
    describe "my_minimum" $
        prop "behaves like model" $ \(xs :: [Integer]) -> xs /= []
          ==> my_minimum xs `shouldBe` minimum xs
    describe "my_minimum_iter" $
        prop "behaves like model" $ \(xs :: [Integer]) -> xs /= []
          ==> my_minimum_iter xs `shouldBe` minimum xs
    describe "my_and" $
        prop "behaves like model" $ \(xs :: [Bool]) ->
           my_and xs `shouldBe` and xs
    describe "my_and_bad_iter" $
        prop "behaves like model" $ \(xs :: [Bool]) ->
           my_and_bad_iter xs `shouldBe` and xs
    describe "my_and_iter" $
        prop "behaves like model" $ \(xs :: [Bool]) ->
           my_and_iter xs `shouldBe` and xs
    describe "my_or" $
        prop "behaves like model" $ \(xs :: [Bool]) ->
           my_or xs `shouldBe` or xs
    describe "my_or_iter" $
        prop "behaves like model" $ \(xs :: [Bool]) ->
           my_or_iter xs `shouldBe` or xs
    describe "my_all" $
        prop "behaves like model" $ \(xs :: [Integer]) -> do
           my_all even xs `shouldBe` all even xs
           my_all odd  xs `shouldBe` all odd  xs
           my_all (>1) xs `shouldBe` all (>1) xs
    describe "my_all_iter" $
        prop "behaves like model" $ \(xs :: [Integer]) -> do
           my_all_iter even xs `shouldBe` all even xs
           my_all_iter odd  xs `shouldBe` all odd  xs
           my_all_iter (>1) xs `shouldBe` all (>1) xs
    describe "my_elem" $
        prop "behaves like model" $ \(k :: Integer) (xs :: [Integer]) ->
           my_elem k xs `shouldBe` elem k xs
    describe "my_find" $
        prop "behaves like model" $ \(xs :: [Integer]) -> do
           my_find even xs `shouldBe` find even xs
           my_find odd  xs `shouldBe` find odd  xs
           my_find (>1) xs `shouldBe` find (>1) xs
    describe "my_lookup" $
        prop "behaves like model" $ \(k :: Integer) (xs :: [(Integer,Integer)]) ->
           my_lookup k xs `shouldBe` lookup k xs
    describe "my_foldr" $
        prop "behaves like model" $ \(xs :: [Integer]) -> do
           my_foldr (+) 0 xs `shouldBe` sum xs
           my_foldr (*) 1 xs `shouldBe` product xs
    describe "my_foldl" $
        prop "behaves like model" $ \(xs :: [Integer]) -> do
           my_foldl (+) 0 xs `shouldBe` sum xs
           my_foldl (*) 1 xs `shouldBe` product xs

----------------------------------------------------------------

my_length :: [a] -> Integer
my_length []     = 0
my_length (_:xs) = my_length xs + 1

my_length_iter :: [a] -> Integer
my_length_iter as = iter as 0
  where
    iter :: [a] -> Integer -> Integer
    iter []     n = n
    iter (_:xs) n = iter xs (n + 1)

----------------------------------------------------------------

my_sum :: [Integer] -> Integer
my_sum []     = 0
my_sum (x:xs) = my_sum xs + x

my_sum_iter :: [Integer] -> Integer
my_sum_iter as = iter as 0
  where
    iter []     acc = acc
    iter (x:xs) acc = iter xs (acc + x)

my_product :: [Integer] -> Integer
my_product []     = 1
my_product (x:xs) = my_product xs * x

my_product_iter :: [Integer] -> Integer
my_product_iter as = iter as 1
  where
    iter []     acc = acc
    iter (x:xs) acc = iter xs (acc * x)

----------------------------------------------------------------

my_maximum :: Ord a => [a] -> a
my_maximum []  = error "my_maximum"
my_maximum [x] = x
my_maximum (x:xs)
  | x > y      = x
  | otherwise  = y
  where
    y = my_maximum xs

my_maximum_iter :: Ord a => [a] -> a
my_maximum_iter []  = error "my_maximum_iter"
my_maximum_iter (a:as) = iter as a
  where
    iter []     acc = acc
    iter (x:xs) acc
      | x > acc     = iter xs x
      | otherwise   = iter xs acc

my_minimum :: Ord a => [a] -> a
my_minimum []  = error "my_minimum"
my_minimum [x] = x
my_minimum (x:xs)
  | y < x      = y
  | otherwise  = x
  where
    y = my_minimum xs

my_minimum_iter :: Ord a => [a] -> a
my_minimum_iter []  = error "my_minimum_iter"
my_minimum_iter (a:as) = iter as a
  where
    iter []     acc = acc
    iter (x:xs) acc
      | x < acc     = iter xs x
      | otherwise   = iter xs acc

----------------------------------------------------------------

my_and :: [Bool] -> Bool
my_and []     = True
my_and (x:xs) = x && my_and xs

my_and_bad_iter :: [Bool] -> Bool
my_and_bad_iter as = iter as True
  where
    iter []     acc = acc
    iter (x:xs) acc = iter xs (x && acc)

my_and_iter :: [Bool] -> Bool
my_and_iter [] = True
my_and_iter (x:xs)
  | x          = my_and_iter xs
  | otherwise  = False

my_or :: [Bool] -> Bool
my_or []     = False
my_or (x:xs) = x || my_or xs

my_or_iter :: [Bool] -> Bool
my_or_iter [] = False
my_or_iter (x:xs)
  | x         = True
  | otherwise = my_or_iter xs

my_all :: (a -> Bool) -> [a] -> Bool
my_all _ []     = True
my_all p (x:xs) = p x && my_all p xs

my_all_iter :: (a -> Bool) -> [a] -> Bool
my_all_iter _ []     = True
my_all_iter p (x:xs)
  | p x       = my_all_iter p xs
  | otherwise = False

----------------------------------------------------------------

my_elem :: Eq a => a -> [a] -> Bool
my_elem _ []  = False
my_elem k (x:xs)
  | k == x    = True
  | otherwise = my_elem k xs

my_find :: (a -> Bool) -> [a] -> Maybe a
my_find _ [] = Nothing
my_find p (x:xs)
  | p x       = Just x
  | otherwise = my_find p xs

my_lookup :: Eq k => k -> [(k,v)] -> Maybe v
my_lookup _ []           = Nothing
my_lookup key ((k,v):kvs)
    | key == k           = Just v
    | otherwise          = my_lookup key kvs

----------------------------------------------------------------

my_foldr :: (a -> b -> b) -> b -> [a] -> b
my_foldr _ ini []     = ini
my_foldr f ini (x:xs) = f x (my_foldr f ini xs)

my_foldl :: (a -> b -> a) -> a -> [b] -> a
my_foldl _ acc []     = acc
my_foldl f acc (x:xs) = my_foldl f (f acc x) xs
