{-#LANGUAGE
   FlexibleInstances
  , TypeSynonymInstances
  , GeneralizedNewtypeDeriving
  #-}
-- | Simple tests for applicative laws of the Result type
module Text.Digestive.Field.QTests
       ( tests
       ) where


--------------------------------------------------------------------------------
import Test.Framework (Test, testGroup)
import Test.QuickCheck hiding (Result, Success)
import Test.Framework.Providers.QuickCheck2 (testProperty)


--------------------------------------------------------------------------------
import Text.Digestive.Types


--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Text.Digestive.Field.QTests"
        [
           testProperty "Applicative identity for Result"
             prop_resappid
          ,testProperty "Applicative compositionality for Result"
             prop_resappcomp
        ]


--------------------------------------------------------------------------------
instance (Arbitrary v, Arbitrary a) => Arbitrary (Result a v) where
  arbitrary = oneof [liftM Success arbitrary, liftM Error arbitrary]


--------------------------------------------------------------------------------
-- Eq instance for convenience
instance (Eq a, Eq v) => Eq (Result a v)
   where (Success a) == (Success a') = a == a'
         (Error v)   == (Error v')   = v == v'
         _           == _            = False

--------------------------------------------------------------------------------
type ResFunc a = Result [Int] a


--------------------------------------------------------------------------------
instance Show (Int -> Int)
   where show _ = ""


--------------------------------------------------------------------------------
-- Identity (pure id <*> f == f)
prop_resappid :: Result [Int] Int -> Bool
prop_resappid f = (pure id <*> f) == f


--------------------------------------------------------------------------------
-- Compositionality (pure (.) <*> u <*> v <*> w == u<*>(v<*>w))
prop_resappcomp  ::  ResFunc (Int -> Int) -> ResFunc (Int -> Int) ->
                     Result [Int] (Int) -> Bool
prop_resappcomp u v w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

