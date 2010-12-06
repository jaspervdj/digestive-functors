module Text.Digestive.Http
    ( HttpInput (..)
    , inputString
    , inputRead
    , inputBool
    , inputChoice
    , inputFile
    ) where

import Control.Applicative ((<$>))
import Control.Monad (mplus)
import Data.Monoid (Monoid, mconcat)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Lazy as LB

import Text.Digestive.Common
import Text.Digestive.Types
import Text.Digestive.Result
import Text.Digestive.Transform

class HttpInput a where
    getInputString :: a -> String
    getInputFile :: a -> (String, LB.ByteString)

inputString :: (Monad m, Functor m, HttpInput i)
            => (FormId -> Maybe String -> v)  -- ^ View constructor
            -> Maybe String                   -- ^ Default value
            -> Form m i e v String            -- ^ Resulting form
inputString = input toView toResult
  where
    toView _ inp defaultInput = fmap getInputString inp `mplus` defaultInput
    toResult = const . Ok . fromMaybe "" . fmap getInputString

inputRead :: (Monad m, Functor m, HttpInput i, Read a, Show a)
          => (FormId -> Maybe String -> v)  -- ^ View constructor
          -> e                              -- ^ Error when no read
          -> Maybe a                        -- ^ Default input
          -> Form m i e v a                 -- ^ Resulting form
inputRead cons' error' def = inputString cons' (fmap show def)
    `transform` transformRead error'

inputBool :: (Monad m, Functor m, HttpInput i)
          => (FormId -> Bool -> v)   -- ^ View constructor
          -> Bool                    -- ^ Default input
          -> Form m i e v Bool       -- ^ Resulting form
inputBool = input toView toResult
  where
    toView isInput inp def = if isInput then readBool inp else def
    toResult inp _ = Ok $ readBool inp
    readBool (Just x) = not (null $ getInputString x)
    readBool Nothing  = False

inputChoice :: (Monad m, Functor m, HttpInput i, Monoid v, Eq a)
            => (FormId -> String -> Bool -> a -> v)  -- ^ Choice constructor
            -> a                                     -- ^ Default option
            -> [a]                                   -- ^ Choices
            -> Form m i e v a                        -- ^ Resulting form
inputChoice toView defaultInput choices = Form $ do
    inputKey <- fromMaybe "" . fmap getInputString <$> getFormInput
    id' <- getFormId
    let -- Find the actual input, based on the key, or use the default input
        inp = fromMaybe defaultInput $ lookup inputKey $ zip (ids id') choices
        -- Apply the toView' function to all choices
        view' = mconcat $ zipWith (toView' id' inp) (ids id') choices
    return (View (const view'), Ok inp)
  where
    ids id' = map (((show id' ++ "-") ++) . show) [1 .. length choices]
    toView' id' inp key x = toView id' key (inp == x) x

inputFile :: (Monad m, Functor m, HttpInput i)
          => (FormId -> v)                                 -- ^ View constructor
          -> Form m i e v (Maybe (String, LB.ByteString))  -- ^ Resulting form
inputFile viewCons = input toView toResult viewCons' ()
  where
    toView _ _ _ = ()
    toResult inp _ = Ok $ fmap getInputFile inp
    viewCons' id' () = viewCons id'
