{-# LANGUAGE GADTs #-}
module Text.Digestive.Form.Encoding
    ( FormEncType (..)
    , formEncType
    , formTreeEncType
    ) where

import Control.Monad (liftM)
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

fieldList :: FormTree Identity v m a -> [SomeField v]
fieldList = mapMaybe toField' . fieldList' . SomeForm
  where
    fieldList' (SomeForm f) = SomeForm f : concatMap fieldList' (children f)
    toField' (SomeForm f)   = toField f

formEncType :: Monad m => Form v m a -> m FormEncType
formEncType form = liftM formTreeEncType $ toFormTree form

formTreeEncType :: FormTree Identity v m a -> FormEncType
formTreeEncType = mconcat . map fieldEncType' . fieldList
  where
    fieldEncType' (SomeField f) = fieldEncType f
