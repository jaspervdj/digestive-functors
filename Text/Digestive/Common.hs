-- | Functions to construct common forms
--
module Text.Digestive.Common
    ( input
    , inputString
    , inputRead
    , inputBool
    , inputChoice
    , label
    , errors
    , childErrors
    ) where

import Control.Applicative ((<$>))
import Control.Monad (mplus)
import Data.Monoid (Monoid, mconcat)
import Data.Maybe (fromMaybe)

import Text.Digestive.Types
import Text.Digestive.Result
import Text.Digestive.Transform

input :: (Monad m, Functor m)
      => (Bool -> Maybe i -> d -> s)           -- ^ Get the viewed result
      -> (Maybe i -> FormRange -> Result e a)  -- ^ Get the returned result
      -> (FormId -> s -> v)                    -- ^ View constructor
      -> d                                     -- ^ Default value
      -> Form m i e v a                        -- ^ Resulting form
input toView toResult createView defaultInput = Form $ do
    isInput <- isFormInput
    inp <- getFormInput
    id' <- getFormId
    range <- getFormRange
    let view' = toView isInput inp defaultInput
        result' = toResult inp range
    return (View (const $ createView id' view'), result')

inputString :: (Monad m, Functor m)
            => (FormId -> Maybe String -> v)  -- ^ View constructor
            -> Maybe String                   -- ^ Default value
            -> Form m String e v String       -- ^ Resulting form
inputString = input toView toResult
  where
    toView = const mplus
    toResult = const . Ok . fromMaybe ""

inputRead :: (Monad m, Functor m, Read a, Show a)
          => (FormId -> Maybe String -> v)  -- ^ View constructor
          -> e                              -- ^ Error when no read
          -> Maybe a                        -- ^ Default input
          -> Form m String e v a            -- ^ Resulting form
inputRead cons' error' def = inputString cons' (fmap show def)
    `transform` transformRead error'

inputBool :: (Monad m, Functor m)
          => (FormId -> Bool -> v)   -- ^ View constructor
          -> Bool                    -- ^ Default input
          -> Form m String e v Bool  -- ^ Resulting form
inputBool = input toView toResult
  where
    toView isInput inp def = if isInput then readBool inp else def
    toResult inp _ = Ok $ readBool inp
    readBool (Just x) = not (null x)
    readBool Nothing  = False

inputChoice :: (Monad m, Functor m, Monoid v, Eq a)
            => (FormId -> String -> Bool -> a -> v)  -- ^ Choice constructor
            -> a                                     -- ^ Default option
            -> [a]                                   -- ^ Choices
            -> Form m String e v a                   -- ^ Resulting form
inputChoice toView defaultInput choices = Form $ do
    inputKey <- fromMaybe "" <$> getFormInput
    id' <- getFormId
    let -- Find the actual input, based on the key, or use the default input
        inp = fromMaybe defaultInput $ lookup inputKey $ zip (ids id') choices
        -- Apply the toView' function to all choices
        view' = mconcat $ zipWith (toView' id' inp) (ids id') choices
    return (View (const view'), Ok inp)
  where
    ids id' = map (((show id' ++ "-") ++) . show) [1 .. length choices]
    toView' id' inp key x = toView id' key (inp == x) x

label :: Monad m
      => (FormId -> v)
      -> Form m i e v ()
label f = Form $ do
    id' <- getFormId
    return (View (const $ f id'), Ok ())

errors :: Monad m
       => ([e] -> v)
       -> Form m i e v ()
errors f = Form $ do
    range <- getFormRange
    return (View (f . retainErrors range), Ok ())

childErrors :: Monad m
            => ([e] -> v)
            -> Form m i e v ()
childErrors f = Form $ do
    range <- getFormRange
    return (View (f . retainChildErrors range), Ok ())
