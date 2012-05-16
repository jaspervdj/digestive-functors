{-# LANGUAGE GADTs #-}
module Text.Digestive.Form.Encoding
    ( FormEncType (..)
    , formEncType
    ) where

import Control.Monad.Identity (Identity)
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid (..), mconcat)

import Text.Digestive.Field
import Text.Digestive.Form.Internal

data FormEncType
    = UrlEncoded
    | MultiPart
    deriving (Eq)

instance Show FormEncType where
    show UrlEncoded = "application/x-www-form-urlencoded"
    show MultiPart  = "multipart/form-data"

-- Monoid instance for encoding types: prefer UrlEncoded, but fallback to
-- MultiPart when needed
instance Monoid FormEncType where
    mempty               = UrlEncoded
    mappend UrlEncoded x = x
    mappend MultiPart  _ = MultiPart

fieldEncType :: Field v a -> FormEncType
fieldEncType File = MultiPart
fieldEncType _    = UrlEncoded

formEncType :: FormTree Identity v m a -> FormEncType
formEncType = mconcat . map fieldEncType' . fieldList
  where
    fieldEncType' (SomeField f) = fieldEncType f

fieldList :: FormTree Identity v m a -> [SomeField v]
fieldList = mapMaybe toField' . fieldList' . SomeForm
  where
    fieldList' (SomeForm f) = SomeForm f : concatMap fieldList' (children f)
    toField' (SomeForm f)   = toField f
