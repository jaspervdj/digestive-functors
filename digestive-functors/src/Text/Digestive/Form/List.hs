--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-- | Functionality related to index storage and the DefaultList type.
module Text.Digestive.Form.List
    ( indicesRef
    , parseIndices
    , unparseIndices
    , DefaultList (..)
    , defaultListIndex
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>), (<*>))
import           Data.Foldable       (Foldable (..))
import           Data.Maybe          (mapMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Traversable    (Traversable (..))
import           Prelude             hiding (foldr)


--------------------------------------------------------------------------------
import           Text.Digestive.Util


--------------------------------------------------------------------------------
-- | Key used to store list indices
indicesRef :: Text
indicesRef = "indices"


--------------------------------------------------------------------------------
-- | Parse a string of comma-delimited integers to a list.
-- Unparseable substrings are left out of the result.
parseIndices :: Text -> [Int]
parseIndices = mapMaybe (readMaybe . T.unpack) . T.split (== ',')


--------------------------------------------------------------------------------
-- | Serialize a list of integers as a comma-delimited Text
unparseIndices :: [Int] -> Text
unparseIndices = T.intercalate "," . map (T.pack . show)


--------------------------------------------------------------------------------
-- | A list which, when indexed on non-existant positions, returns a default
-- value.
data DefaultList a = DefaultList a [a]


--------------------------------------------------------------------------------
instance Functor DefaultList where
    fmap f (DefaultList x xs) = DefaultList (f x) (map f xs)


--------------------------------------------------------------------------------
instance Foldable DefaultList where
    foldr f z (DefaultList x xs) = f x (foldr f z xs)


--------------------------------------------------------------------------------
instance Traversable DefaultList where
    traverse f (DefaultList x xs) = DefaultList <$> f x <*> traverse f xs


--------------------------------------------------------------------------------
-- | Safe indexing of a DefaultList - returns the default value if
-- the given index is out of bounds.
defaultListIndex :: DefaultList a -> Int -> a
defaultListIndex (DefaultList x xs) i
    | i < 0     = x
    | otherwise = case drop i xs of
        (y : _) -> y
        []      -> x
