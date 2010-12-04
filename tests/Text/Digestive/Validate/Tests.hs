module Text.Digestive.Validate.Tests
    ( tests
    ) where

import Data.Monoid (mempty)

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit ()

import Text.Digestive.Tests.Util
import Text.Digestive.Types
import Text.Digestive.Result
import Text.Digestive.Validate

tests :: [Test]
tests = [ testProperty "always fails"     alwaysFails
        , testProperty "always validates" alwaysValidates
        ]

alwaysFails :: Form Id i String Int Int -> Bool
alwaysFails f = case snd (runIdForm $ f `validate` v) of
    Error l -> "Always fails" `elem` map snd l
    _ -> False
  where
    v = check "Always fails" $ const False

alwaysValidates :: Form Id i String Int Int -> Bool
alwaysValidates f = snd (runIdForm f) == snd (runIdForm $ f `validate` v)
  where
    v = check "Always validates" $ const True
