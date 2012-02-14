module Text.Digestive.Util
    ( readMaybe
    , bindResult
    ) where

import Text.Digestive.Types

readMaybe :: Read a => String -> Maybe a
readMaybe str = case readsPrec 1 str of
    [(x, "")] -> Just x
    _         -> Nothing

bindResult :: Monad m
           => m (Result v a) -> (a -> m (Result v b)) -> m (Result v b)
bindResult mx f = do
    x <- mx
    case x of
        Error errs  -> return $ Error errs
        Success x'  -> f x'
