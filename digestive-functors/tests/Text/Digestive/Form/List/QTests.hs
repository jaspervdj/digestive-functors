module Text.Digestive.Form.List.QTests
       ( tests
       ) where

--------------------------------------------------------------------------------
import Test.Framework
import Test.Framework.Providers.QuickCheck2


--------------------------------------------------------------------------------
import Text.Digestive.Form.List


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Text.Digestive.Types.Tests"
    [
       testProperty "Parsing and serializing sanity check" prop_parseSym
    ]


--------------------------------------------------------------------------------
--  Parsing serialized indices yields the same indices
prop_parseSym :: [Int] -> Bool
prop_parseSym is = (==is) . parseIndices . unparseIndices $ is
