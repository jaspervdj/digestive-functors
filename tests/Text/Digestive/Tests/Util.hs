{-# LANGUAGE FlexibleContexts #-}
module Text.Digestive.Tests.Util
    ( Id (..)
    , runIdForm
    ) where

import Data.Monoid (mempty)
import Control.Applicative (Applicative (..), (<$>))
import Test.QuickCheck (Arbitrary (..))

import Text.Digestive.Result
import Text.Digestive.Types

-- | An ID Monad
--
newtype Id a = Id {unId :: a}
             deriving (Show)

instance Functor Id where
    fmap f (Id x) = Id $ f x

instance Applicative Id where
    pure = Id
    (Id f) <*> (Id x) = Id $ f x

instance Monad Id where
    return = Id
    (Id x) >>= f = f x

-- | Run an ID form without input
--
runIdForm :: Form Id i e v a -> (View e v, Result e a)
runIdForm form = unId $ runForm form "form" mempty

instance (Monad m, Show (m v)) => Show (Form m i e v a) where
    show form = show $ viewForm form "show-form"

instance Arbitrary a => Arbitrary (Result e a) where
    arbitrary = Ok <$> arbitrary

instance Arbitrary v => Arbitrary (View e v) where
    arbitrary = View . const <$> arbitrary

instance (Monad m, Arbitrary v, Arbitrary a) => Arbitrary (Form m i e v a) where
    arbitrary = do
        v <- arbitrary
        r <- arbitrary
        return $ Form $ return (v, r)
