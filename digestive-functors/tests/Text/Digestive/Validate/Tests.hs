module Text.Digestive.Validate.Tests
    ( tests
    ) where

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Text.Digestive.Tests.Util
import Text.Digestive.Types
import Text.Digestive.Result
import Text.Digestive.Validate

tests :: [Test]
tests = [ testProperty "always fails"     alwaysFails
        , testProperty "always validates" alwaysValidates
        , testProperty "simple validator" simpleValidator
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

simpleValidator :: Form Id i String Int Int -> Bool
simpleValidator f = case snd (runIdForm $ f `validate` v) of
    Error _ -> True
    Ok x -> x > 2
  where
    v = check "Must be bigger than 2" (> 2)
