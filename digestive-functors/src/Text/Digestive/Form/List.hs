--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Form.List
    ( parseIndices
    , unparseIndices
    ) where


--------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T


--------------------------------------------------------------------------------
parseIndices :: Text -> [Text]
parseIndices = T.split (== ',')


--------------------------------------------------------------------------------
unparseIndices :: [Text] -> Text
unparseIndices = T.intercalate ","
