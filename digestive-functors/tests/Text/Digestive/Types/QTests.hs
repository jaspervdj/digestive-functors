module Text.Digestive.Types.QTests
       ( tests
       ) where

--------------------------------------------------------------------------------
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck


--------------------------------------------------------------------------------
import Text.Digestive.Types


--------------------------------------------------------------------------------
import Data.Text
import Control.Monad


--------------------------------------------------------------------------------

tests :: Test
tests = testGroup "Text.Digestive.Types.Tests"
    [
     testProperty "Path parsing and serializing" prop_parseSym
    ]

--------------------------------------------------------------------------------
-- This property does not hold in the current implementation,
-- ".a.b...c." will yield the same path as "a.b.c"
prop_parseSym :: PText -> Bool
prop_parseSym pt = (==p) . fromPath . toPath $ p
   where p = runPT pt


--------------------------------------------------------------------------------
newtype PText = PT {runPT :: Text}
   deriving Show


--------------------------------------------------------------------------------
instance Arbitrary PText
   where arbitrary = liftM PT arbitrary


--------------------------------------------------------------------------------
instance Arbitrary Text
   where arbitrary = liftM pack arbitrary