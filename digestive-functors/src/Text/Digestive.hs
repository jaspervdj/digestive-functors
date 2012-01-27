{-# LANGUAGE GADTs #-}
module Text.Digestive
    (
    ) where

import Control.Applicative (Applicative (..), (<$>))
import Data.Map (Map)
import Data.Monoid (Monoid, mappend)
import qualified Data.Map as M

--------------------------------------------------------------------------------

data Result v a
    = Success a
    | Error v
    deriving (Show)

instance Functor (Result v) where
    fmap f (Success x) = Success (f x)
    fmap _ (Error x)   = Error x

instance Monoid v => Applicative (Result v) where
    pure x                  = Success x
    Error x   <*> Error y   = Error $ mappend x y
    Error x   <*> Success _ = Error x
    Success _ <*> Error y   = Error y
    Success x <*> Success y = Success (x y)

--------------------------------------------------------------------------------

newtype Validate i v a = Validate {unValidate :: i -> Result v a}

instance Functor (Validate i v) where
    fmap f (Validate x) = Validate (fmap f . x)

instance Monoid v => Applicative (Validate i v) where
    pure x                    = Validate $ const $ pure x
    Validate x <*> Validate y = Validate $ \i -> x i <*> y i

--------------------------------------------------------------------------------

type Ref = [String]

data Form i v a where
    Pure :: Ref -> Validate i v a -> Form i v a
    App  :: Ref -> Form i v (b -> a) -> Form i v b -> Form i v a 

instance Show (Form i v a) where
    show (Pure r _)  = "(Pure " ++ show r ++ " _)"
    show (App r x y) =
        "(App " ++ show r ++ " " ++ show x ++ " " ++ show y ++ ")"

instance Functor (Form i v) where
    fmap f (Pure r x)  = Pure r (fmap f x)
    fmap f (App r x y) = App r (fmap (f .) x) y

instance Monoid v => Applicative (Form i v) where
    pure x  = Pure [] (pure x)
    x <*> y = App [] x y

ref :: String -> Form i v a -> Form i v a
ref r (Pure _ x)  = Pure [r] x
ref r (App _ x y) = App [r] x y

--------------------------------------------------------------------------------

eval :: i -> Form i v a -> Either () a
eval = undefined

--------------------------------------------------------------------------------

data User = User String Int
    deriving (Show)

userForm :: Form i String User
userForm = User
    <$> ref "name" (pure "jasper")
    <*> ref "age" (pure 21)
