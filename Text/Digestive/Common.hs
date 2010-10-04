-- | Functions to construct common forms
--
module Text.Digestive.Common where

import Control.Applicative ((<$>))
import Control.Monad (mplus)
import Data.Maybe (fromMaybe)

import Text.Digestive.Types

readMaybe :: Read a => String -> Maybe a
readMaybe string = case readsPrec 1 string of
    [(x, "")] -> Just x
    _ -> Nothing

inputString :: (Monad m, Functor m)
            => Maybe String
            -> (FormId -> Maybe String -> v)
            -> Form m String e v String
inputString defaultInput createView = Form $ do
    inp <- (`mplus` defaultInput) <$> getFormInput
    id' <- getFormId
    return (View (const $ createView id' inp), Ok $ fromMaybe "" inp)

inputRead :: (Monad m, Functor m, Read a, Show a)
          => e                              -- ^ Error when no read
          -> Maybe a                        -- ^ Default input
          -> (FormId -> Maybe String -> v)  -- ^ View constructor
          -> Form m String e v a            -- ^ Resulting form
inputRead error' defaultInput createView = Form $ do
    range <- getFormRange
    (view', result) <- unForm $ inputString defaultInput' createView
    return (view', result >>= readResult range)
  where
    defaultInput' = show <$> defaultInput
    readResult range x = case readMaybe x of
        Nothing -> Error [(range, error')]
        Just y  -> Ok y
