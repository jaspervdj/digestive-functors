module Main
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Text.Digestive.Field.Tests (tests)
import qualified Text.Digestive.Field.QTests (tests)
import qualified Text.Digestive.Form.Encoding.Tests (tests)
import qualified Text.Digestive.View.Tests (tests)
import qualified Text.Digestive.Form.QTests (tests)
import qualified Text.Digestive.Form.Encoding.QTests (tests)
import qualified Text.Digestive.Form.List.QTests (tests)



main :: IO ()
main = defaultMain
    [ Text.Digestive.Field.Tests.tests
    , Text.Digestive.Field.QTests.tests
    , Text.Digestive.Form.Encoding.Tests.tests
    , Text.Digestive.View.Tests.tests
    , Text.Digestive.Form.QTests.tests
    , Text.Digestive.Form.Encoding.QTests.tests
    , Text.Digestive.Form.List.QTests.tests
    ]
