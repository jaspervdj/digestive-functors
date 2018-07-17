--------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
-- | Provides a datatype to differentiate between regular urlencoding and
-- multipart encoding for the content of forms and functions to determine
-- the content types of forms.
module Text.Digestive.Form.Encoding
    ( FormEncType (..)
    , formTreeEncType
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Identity             (Identity)
import           Data.Maybe                         (mapMaybe)
import           Data.Monoid                        (Monoid (..), mconcat)


--------------------------------------------------------------------------------
import           Text.Digestive.Form.Internal
import           Text.Digestive.Form.Internal.Field


--------------------------------------------------------------------------------
-- | Content type encoding of the form, either url encoded
-- (percent-encoding) or multipart encoding. For details, see:
-- <http://www.w3.org/TR/html401/interact/forms.html#h-17.13.4>
data FormEncType
    = UrlEncoded
    | MultiPart
    deriving (Eq)


--------------------------------------------------------------------------------
instance Show FormEncType where
    show UrlEncoded = "application/x-www-form-urlencoded"
    show MultiPart  = "multipart/form-data"


--------------------------------------------------------------------------------
-- | @'SemiGroup' 'Monoid'@ instance for encoding types: prefer
-- @UrlEncoded@, but fallback to @MultiPart@ when needed
instance Semigroup FormEncType where
    UrlEncoded <> x = x
    MultiPart  <> _ = MultiPart

instance Monoid FormEncType where
    mempty               = UrlEncoded


--------------------------------------------------------------------------------
-- Only file uploads require the multipart encoding
fieldEncType :: Field v a -> FormEncType
fieldEncType File = MultiPart
fieldEncType _    = UrlEncoded


--------------------------------------------------------------------------------
-- Retrieves all fields from a form tree
fieldList :: FormTree Identity v m a -> [SomeField v]
fieldList = mapMaybe toField' . fieldList' . SomeForm
  where
    fieldList' (SomeForm f) = SomeForm f : concatMap fieldList' (children f)
    toField' (SomeForm f)   = toField f


--------------------------------------------------------------------------------
-- | Determines the encoding type of a "FormTree"
formTreeEncType :: FormTree Identity v m a -> FormEncType
formTreeEncType = mconcat . map fieldEncType' . fieldList
  where
    fieldEncType' (SomeField f) = fieldEncType f
