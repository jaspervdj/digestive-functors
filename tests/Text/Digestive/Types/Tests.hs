module Text.Digestive.Types.Tests
    ( tests
    ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Text.Digestive.Tests.Util
import Text.Digestive.Types

tests :: [Test]
tests = [ testProperty "view only"    viewOnly
        , testProperty "left append"  leftAppend
        , testProperty "right append" rightAppend
        ]

-- If we only have a view as form, that should exactly be the view we get
-- back...
viewOnly :: String -> Bool
viewOnly str = str == unId (viewForm (view str) "form")

-- Check that the views get appended...
leftAppend :: Form Id i e String Int -> Form Id i e String () -> Bool
leftAppend f1 f2 = unId (viewForm f1 "form") ++ unId (viewForm f2 "form") ==
    unId (viewForm (f1 <++ f2) "form")

-- Same for right append
rightAppend :: Form Id i e String () -> Form Id i e String Int -> Bool
rightAppend f1 f2 = unId (viewForm f1 "form") ++ unId (viewForm f2 "form") ==
    unId (viewForm (f1 ++> f2) "form")
