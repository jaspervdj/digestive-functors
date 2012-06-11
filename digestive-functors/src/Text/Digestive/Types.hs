--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Types
    ( Result (..)
    , resultMapError
    , Path
    , toPath
    , fromPath
    , Method (..)
    , FormInput (..)
    , Env
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative (Applicative(..))
import           Data.Monoid         (Monoid, mappend)


--------------------------------------------------------------------------------
import           Data.Text           (Text)
import qualified Data.Text           as T


--------------------------------------------------------------------------------
-- | A mostly internally used type for representing Success/Error, with a
-- special applicative instance
data Result v a
    = Success a
    | Error v
    deriving (Show)


--------------------------------------------------------------------------------
instance Functor (Result v) where
    fmap f (Success x) = Success (f x)
    fmap _ (Error x)   = Error x


--------------------------------------------------------------------------------
instance Monoid v => Applicative (Result v) where
    pure x                  = Success x
    Error x   <*> Error y   = Error $ mappend x y
    Error x   <*> Success _ = Error x
    Success _ <*> Error y   = Error y
    Success x <*> Success y = Success (x y)


--------------------------------------------------------------------------------
instance Monad (Result v) where
    return x          = Success x
    (Error x)   >>= _ = Error x
    (Success x) >>= f = f x


--------------------------------------------------------------------------------
-- | Map over the error type of a 'Result'
resultMapError :: (v -> w) -> Result v a -> Result w a
resultMapError f (Error x)   = Error (f x)
resultMapError _ (Success x) = Success x


--------------------------------------------------------------------------------
-- | Describes a path to a subform
type Path = [Text]


--------------------------------------------------------------------------------
-- | Create a 'Path' from some text
toPath :: Text -> Path
toPath = filter (not . T.null) . T.split (== '.')


--------------------------------------------------------------------------------
-- | Serialize a 'Path' to 'Text'
fromPath :: Path -> Text
fromPath = T.intercalate "."


--------------------------------------------------------------------------------
-- | The HTTP methods
data Method = Get | Post
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
-- | The different input types sent by the browser
data FormInput
    = TextInput Text
    | FileInput FilePath
    deriving (Show)


--------------------------------------------------------------------------------
-- | An environment (e.g. a server) from which we can read input parameters. A
-- single key might be associated with multiple text values (multi-select).
type Env m = Path -> m [FormInput]
