--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
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
indicesRef :: Text
indicesRef = "indices"


--------------------------------------------------------------------------------
parseIndices :: Text -> [Int]
parseIndices = mapMaybe (readMaybe . T.unpack) . T.split (== ',')


--------------------------------------------------------------------------------
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
defaultListIndex :: DefaultList a -> Int -> a
defaultListIndex (DefaultList x xs) i
    | i < 0     = x
    | otherwise = case drop i xs of
        (y : _) -> y
        []      -> x
