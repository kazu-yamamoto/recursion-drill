module Small (
    Small(..)
  , module Test.Hspec
  , module Test.Hspec.QuickCheck
  , module Test.QuickCheck
  ) where

import Control.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Small)

newtype Small = Small Integer deriving Show

instance Arbitrary Small where
    arbitrary = Small . (`mod` 10) <$> arbitrary
