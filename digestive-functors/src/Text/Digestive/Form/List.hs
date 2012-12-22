--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Form.List
    ( indicesRef
    , parseIndices
    , unparseIndices
    ) where


--------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T


--------------------------------------------------------------------------------
indicesRef :: Text
indicesRef = "indices"


--------------------------------------------------------------------------------
parseIndices :: Text -> [Text]
parseIndices = T.split (== ',')


--------------------------------------------------------------------------------
unparseIndices :: [Text] -> Text
unparseIndices = T.intercalate ","
