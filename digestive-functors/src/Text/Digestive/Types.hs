{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Types
    ( Result (..)
    , Path
    , toPath
    , fromPath
    , Env
    ) where

import Control.Applicative (Applicative (..))
import Data.Monoid (Monoid, mappend)

import Data.Text (Text)
import qualified Data.Text as T

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

instance Monad (Result v) where
    return x          = Success x
    (Error x)   >>= _ = Error x
    (Success x) >>= f = f x

type Path = [Text]

toPath :: Text -> Path
toPath = T.split (== '.')

fromPath :: Path -> Text
fromPath = T.intercalate "."

type Env m = Path -> m (Maybe Text)
