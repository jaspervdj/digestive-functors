--------------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Digestive.Form.Encoding.QTests
   ( tests
   ) where


--------------------------------------------------------------------------------
import           Data.Monoid
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck


--------------------------------------------------------------------------------
import           Text.Digestive.Form.Encoding


--------------------------------------------------------------------------------
-- Monoid properties
tests :: Test
tests = testGroup "Text.Digestive.Types.Tests"
    [
         testProperty "enctype monoid - Left identity" prop_idl
       , testProperty "enctype monoid - Right identity" prop_idr
       , testProperty "enctype monoid - Associativity" prop_assoc
    ]


--------------------------------------------------------------------------------
prop_idl :: FormEncType -> Bool
prop_idl a = a `mappend` mempty == a


--------------------------------------------------------------------------------
prop_idr :: FormEncType -> Bool
prop_idr a = mempty `mappend` a == a


--------------------------------------------------------------------------------
prop_assoc :: FormEncType -> FormEncType -> FormEncType -> Bool
prop_assoc a b c = lhs == rhs
   where lhs = a `mappend` b `mappend` c
         rhs = a `mappend` (b `mappend` c)


--------------------------------------------------------------------------------
instance Arbitrary FormEncType
   where arbitrary = elements [MultiPart, UrlEncoded]
