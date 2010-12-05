-- | Main test module
--
module TestSuite
    ( main
    ) where

import Test.Framework (defaultMain, testGroup)

import qualified Text.Digestive.Validate.Tests (tests)
import qualified Text.Digestive.Common.Tests (tests)

main :: IO ()
main = defaultMain
    [ testGroup "Text.Digestive.Validate.Tests"
        Text.Digestive.Validate.Tests.tests
    , testGroup "Text.Digestive.Common.Tests"
        Text.Digestive.Common.Tests.tests
    ]
