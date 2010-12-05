module Text.Digestive.Types.Tests
    ( tests
    ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Text.Digestive.Tests.Util
import Text.Digestive.Types

tests :: [Test]
tests = [ testProperty "viewOnly" viewOnly
        ]

-- If we only have a view as form, that should exactly be the view we get
-- back...
viewOnly :: String -> Bool
viewOnly str = str == unId (viewForm (view str) "form")
