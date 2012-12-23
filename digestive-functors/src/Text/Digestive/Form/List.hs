--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Form.List
    ( indicesRef
    , parseIndices
    , unparseIndices
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe          (mapMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T


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
