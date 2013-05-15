--------------------------------------------------------------------------------
-- | Utilities for safe failable parsing
module Text.Digestive.Util
    ( readMaybe
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe (listToMaybe)


--------------------------------------------------------------------------------
-- | 'read' in the 'Maybe' monad.
readMaybe :: Read a => String -> Maybe a
readMaybe str = listToMaybe [x | (x, "") <- readsPrec 1 str]
