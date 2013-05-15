{-# LANGUAGE
     GADTs
   , OverloadedStrings
   #-}
module Text.Digestive.Form.QTests
       ( tests
       ) where

--------------------------------------------------------------------------------
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding (Success)


--------------------------------------------------------------------------------
import Text.Digestive.Types
import Text.Digestive.Form.Internal
import Text.Digestive.Form.Internal.Field


--------------------------------------------------------------------------------
import Data.Text (Text,pack)
import Control.Monad
import Control.Monad.Identity
import Data.Maybe

--------------------------------------------------------------------------------

tests :: Test
tests = testGroup "Text.Digestive.Types.Tests"
    [
         testProperty "Mapping consistency" prop_viewcons
       , testProperty "Label consistency - map" prop_refcons
       , testProperty "Child count consistency - label" prop_pushcons
       , testProperty "Labelling consistency - monadic" prop_refmoncons
    ]

--------------------------------------------------------------------------------
-- Mapping on the view does not change the child count
prop_viewcons :: FormTree Identity String Identity Int -> Bool
prop_viewcons ft = (length . children $ ft) ==
                   (length . children $ formMapView (length) ft)


--------------------------------------------------------------------------------
-- Adding a label does not change the child count
prop_pushcons :: FormTree Identity String Identity Int -> Bool
prop_pushcons ft = lc ft == lc ("empty" .: ft)
   where lc = length . children


--------------------------------------------------------------------------------
-- Sanity check - adding a ref and popping it yields the same Result
prop_refcons :: Text -> FormTree Identity String Identity Int -> Bool
prop_refcons ref ft = isJust ref' && fromJust ref' == ref
   where ref' = getRef (ref .: ft)


--------------------------------------------------------------------------------
-- Sanity check - monadic wrap does not affect reference consistency
prop_refmoncons :: Text -> FormTree Identity String Identity Int -> Bool
prop_refmoncons ref ft = isJust ref' && fromJust ref' == ref
   where ref' = getRef (monadic . return $ (ref .: ft))


--------------------------------------------------------------------------------
-- Limited arbitrary instance for form trees
instance (Monad t, Monad m, Arbitrary a) => Arbitrary (FormTree t v m a)
   where arbitrary = sized (innerarb $ liftM Pure arbitrary)
            where innerarb g 0 = g
                  innerarb g n = innerarb g' (n-1)
                     where g' = oneof
                             [
                                 arbitrary >>= \r -> liftM (Ref r) g
                                ,liftM  (Monadic . return) g
                                ,liftM  (Map (return . Success . id)) g
                                ,liftM2  App (liftM (fmap const) g) g
                             ]

--------------------------------------------------------------------------------
-- Arbitrary SomeFields - encompasses all field types except for choice.
instance (Arbitrary v) => Arbitrary (SomeField v)
   where arbitrary =
           oneof [
                 liftM (SomeField . Singleton) (arbitrary :: Gen Int)
               , liftM (SomeField . Text) arbitrary
               , liftM (SomeField . Bool) arbitrary
               , liftM  SomeField $ elements [File]
                 ]


--------------------------------------------------------------------------------
-- Arbitrary Fields - limited to Singleton fields
instance (Arbitrary a) => Arbitrary (Field v a)
   where arbitrary = liftM Singleton (arbitrary)


--------------------------------------------------------------------------------
-- Arbitrary Text - should be factored out
instance Arbitrary Text
   where arbitrary = liftM pack arbitrary

--------------------------------------------------------------------------------
-- Show instance - should probably be moved to Field.hs
instance (Show v) => Show (SomeField v)
   where show (SomeField f) = show f