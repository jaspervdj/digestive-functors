module TestSuite
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Text.Digestive.Field.Tests (tests)
import qualified Text.Digestive.View.Tests (tests)

main :: IO ()
main = defaultMain
    [ Text.Digestive.Field.Tests.tests
    , Text.Digestive.View.Tests.tests
    ]
