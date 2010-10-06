-- | Functions to construct common forms
--
module Text.Digestive.Common where

import Control.Monad (mplus)
import Data.Monoid (mempty)
import Data.Maybe (fromMaybe)

import Text.Digestive.Types

input :: (Monad m, Functor m)
      => (Bool -> Maybe String -> d -> s)           -- ^ Get the viewed result
      -> (Maybe String -> FormRange -> Result e a)  -- ^ Get the returned result
      -> d                                          -- ^ Default value
      -> (FormId -> s -> v)                         -- ^ View constructor
      -> Form m String e v a                        -- ^ Resulting form
input toView toResult defaultInput createView = Form $ do
    isInput <- isFormInput
    inp <- getFormInput
    id' <- getFormId
    range <- getFormRange
    let view' = toView isInput inp defaultInput
        result' = toResult inp range
    return (View (const $ createView id' view'), result')

readMaybe :: Read a => String -> Maybe a
readMaybe string = case readsPrec 1 string of
    [(x, "")] -> Just x
    _ -> Nothing

inputString :: (Monad m, Functor m)
            => Maybe String                   -- ^ Default value
            -> (FormId -> Maybe String -> v)  -- ^ View constructor
            -> Form m String e v String       -- ^ Resulting form
inputString = input toView toResult
  where
    toView = const mplus
    toResult = const . Ok . fromMaybe ""

inputRead :: (Monad m, Functor m, Read a, Show a)
          => e                              -- ^ Error when no read
          -> Maybe a                        -- ^ Default input
          -> (FormId -> Maybe String -> v)  -- ^ View constructor
          -> Form m String e v a            -- ^ Resulting form
inputRead error' = input toView toResult
  where
    toView _ inp def = inp `mplus` fmap show def
    toResult inp range = case readMaybe (fromMaybe "" inp) of
        Nothing -> Error [(range, error')]
        Just y  -> Ok y

inputBool :: (Monad m, Functor m)
          => Bool                    -- ^ Default input
          -> (FormId -> Bool -> v)   -- ^ View constructor
          -> Form m String e v Bool  -- ^ Resulting form
inputBool = input toView toResult
  where
    toView isInput inp def = if isInput then readBool inp else def
    toResult inp _ = Ok $ readBool inp
    readBool (Just x) = not (null x)
    readBool Nothing  = False

label :: Monad m
      => (FormId -> v)
      -> Form m i e v a
label f = Form $ do
    id' <- getFormId
    return (View (const $ f id'), mempty)

errors :: Monad m
       => ([e] -> v)
       -> Form m i e v a
errors f = Form $ do
    range <- getFormRange
    return (View (f . retainErrors range), mempty)

childErrors :: Monad m
            => ([e] -> v)
            -> Form m i e v a
childErrors f = Form $ do
    range <- getFormRange
    return (View (f . retainChildErrors range), mempty)
