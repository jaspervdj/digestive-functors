module Text.Digestive.Util
    ( readMaybe
    ) where

readMaybe :: Read a => String -> Maybe a
readMaybe str = case readsPrec 1 str of
    [(x, "")] -> Just x
    _         -> Nothing
