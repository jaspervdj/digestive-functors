{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             NoMonomorphismRestriction #-}
module Text.Digestive.Forms
    ( FormInput (..)
    , inputString
    , inputRead
    , inputBool
    , inputChoice
    , inputFile
    , inputList
    ) where

import Control.Applicative ((<$>))
import Control.Monad (mplus)
import Control.Monad.State (put, get)
import Data.Monoid (Monoid, mappend, mconcat)
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import qualified Data.Text as T (pack)

import Text.Digestive.Common
import Text.Digestive.Types
import Text.Digestive.Result
import Text.Digestive.Transform

-- | Class which all backends should implement. @i@ is here the type that is
-- used to represent a value uploaded by the client in the request
--
class FormInput i f | i -> f where
    -- | Parse the input into a string. This is used for simple text fields
    -- among other things
    --
    getInputString :: i -> Maybe String

    -- | Parse the input value into 'Text'. The default implementation uses
    -- 'T.pack . getInputString', a more efficient version may be implemented.
    --
    getInputText :: i -> Maybe Text
    getInputText = fmap T.pack . getInputString

    -- | Get a file descriptor for an uploaded file
    --
    getInputFile :: i -> Maybe f

inputString :: (Monad m, Functor m, FormInput i f)
            => (FormId -> Maybe String -> v)  -- ^ View constructor
            -> Maybe String                   -- ^ Default value
            -> Form m i e v String            -- ^ Resulting form
inputString = input toView toResult
  where
    toView _ inp defaultInput = (getInputString =<< inp) `mplus` defaultInput
    toResult = Ok . fromMaybe "" . (getInputString =<<)

inputRead :: (Monad m, Functor m, FormInput i f, Read a, Show a)
          => (FormId -> Maybe String -> v)  -- ^ View constructor
          -> e                              -- ^ Error when no read
          -> Maybe a                        -- ^ Default input
          -> Form m i e v a                 -- ^ Resulting form
inputRead cons' error' def = inputString cons' (fmap show def)
    `transform` transformRead error'

inputBool :: (Monad m, Functor m, FormInput i f)
          => (FormId -> Bool -> v)   -- ^ View constructor
          -> Bool                    -- ^ Default input
          -> Form m i e v Bool       -- ^ Resulting form
inputBool = input toView toResult
  where
    toView isInput inp def = if isInput then readBool (getInputString =<< inp)
                                        else def
    toResult inp = Ok $ readBool (getInputString =<< inp)
    readBool (Just x) = not $ null x
    readBool Nothing  = False

inputChoice :: (Monad m, Functor m, FormInput i f, Monoid v, Eq a)
            => (FormId -> String -> Bool -> a -> v)  -- ^ Choice constructor
            -> a                                     -- ^ Default option
            -> [a]                                   -- ^ Choices
            -> Form m i e v a                        -- ^ Resulting form
inputChoice toView defaultInput choices = Form $ do
    inputKey <- fromMaybe "" . (getInputString =<<) <$> getFormInput
    id' <- getFormId
    let -- Find the actual input, based on the key, or use the default input
        inp = fromMaybe defaultInput $ lookup inputKey $ zip (ids id') choices
        -- Apply the toView' function to all choices
        view' = mconcat $ zipWith (toView' id' inp) (ids id') choices
    return (View (const view'), Ok inp)
  where
    ids id' = map (((show id' ++ "-") ++) . show) [1 .. length choices]
    toView' id' inp key x = toView id' key (inp == x) x

inputFile :: (Monad m, Functor m, FormInput i f)
          => (FormId -> v)           -- ^ View constructor
          -> Form m i e v (Maybe f)  -- ^ Resulting form
inputFile viewCons = input toView toResult viewCons' ()
  where
    toView _ _ _ = ()
    toResult inp = Ok $ getInputFile =<< inp
    viewCons' id' () = viewCons id'

up :: Monad m => Int -> FormState m i ()
up n = do
    FormRange s _ <- get
    put $ unitRange $ mapId ((!!n) . iterate tail) s

down :: Monad m => Int -> FormState m i ()
down n = do
    FormRange s _ <- get
    put $ unitRange $ mapId ((!!n) . iterate (0:)) s

-- | Converts a formlet representing a single item into a formlet representing a
-- dynamically sized list of those items.  It requires that the user specify a
-- formlet to hold the length of the list.  Typically this will be a hidden
-- field that is automatically updated by client-side javascript.
--
-- The field names must be generated as follows.  Assume that if inputList had
-- not been used, the field name would have been prefix-f5.  In this case, the
-- list length field name will be prefix-f5.  The first item in the list will
-- receive field names starting at prefix-f5.0.0.  If each item is a composed
-- form with two fields, those fields will have the names prefix-f5.0.0 and
-- prefix-f5.0.1.  The field names for the second item will be prefix-f5.1.0
-- and prefix-f5.1.1.
--
inputList :: (Monad m, Monoid v)
          => Formlet m i e v Int  -- ^ A formlet for the list length
          -> Formlet m i e v a    -- ^ The formlet for a single list element
          -> Formlet m i e v [a]  -- ^ The dynamic list formlet
inputList countField single defaults = Form $ do
    let defCount = maybe 1 length defaults
    (countView,countRes) <- unForm $ countField (Just defCount)
    let countFromForm = getResult countRes
        count = fromMaybe defCount countFromForm
        fs = replicate count single
        forms = zipWith ($) fs $ maybe (maybe [Nothing] (map Just) defaults)
                                       (flip replicate Nothing)
                                       countFromForm
    down 2
    list <- mapM (incAfter . unForm) forms
    up 2
    return ( countView `mappend` (mconcat $ map fst list)
           , combineResults [] [] $ map snd list)
  where
    incAfter k = do
        res <- k
        up 1 >> incState >> down 1
        return res

    combineResults es os [] =
        case es of
            [] -> Ok $ reverse os
            _  -> Error es
    combineResults es os (r:rs) =
        case r of
            Error es' -> combineResults (es ++ es') os rs
            Ok o      -> combineResults es (o:os) rs
