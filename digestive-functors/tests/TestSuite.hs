module TestSuite
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Text.Digestive.View.Tests (tests)

main :: IO ()
main = defaultMain
    [ Text.Digestive.View.Tests.tests
    ]
