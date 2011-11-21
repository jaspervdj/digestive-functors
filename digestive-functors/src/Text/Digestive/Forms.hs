{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             NoMonomorphismRestriction #-}
module Text.Digestive.Forms
    ( FormInput (..)
    , inputString
    , inputText
    , inputRead
    , inputBool
    , inputChoice
    , inputChoices
    , inputFile
    , inputList
    ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM, mplus)
import Control.Monad.State (put, get)
import Control.Monad.Trans (lift)
import Data.Monoid (Monoid, mappend, mconcat)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

import Data.Text (Text)
import qualified Data.Text as T (pack, empty)

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
    getInputString = listToMaybe . getInputStrings

    -- | Should be implemented
    --
    getInputStrings :: i -> [String]

    -- | Parse the input value into 'Text'
    --
    getInputText :: i -> Maybe Text
    getInputText = listToMaybe . getInputTexts

    -- | Can be overriden for efficiency concerns
    --
    getInputTexts :: i -> [Text]
    getInputTexts = map T.pack . getInputStrings

    -- | Get a file descriptor for an uploaded file
    --
    getInputFile :: i -> Maybe f

inputString :: (Monad m, Functor m, FormInput i f)
            => (FormId -> Maybe String -> v)  -- ^ View constructor
            -> Maybe String                   -- ^ Default value
            -> Form m i e v String            -- ^ Resulting form
inputString = input toView toResult
  where
    toView _ inp def = (getInputString =<< inp) `mplus` def
    toResult = Ok . fromMaybe "" . (getInputString =<<)

inputText :: (Monad m, Functor m, FormInput i f)
            => (FormId -> Maybe Text -> v)    -- ^ View constructor
            -> Maybe Text                     -- ^ Default value
            -> Form m i e v Text              -- ^ Resulting form
inputText = input toView toResult
  where
    toView _ inp def = (getInputText =<< inp) `mplus` def
    toResult = Ok . fromMaybe T.empty . (getInputText =<<)

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
    toView isInput inp def
        | isInput   = readBool (getInputString =<< inp)
        | otherwise = def
    toResult inp = Ok $ readBool (getInputString =<< inp)
    readBool (Just x) = not $ null x
    readBool _        = False

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
    return (View (const view'), return (Ok inp))
  where
    ids id' = map (((show id' ++ "-") ++) . show) [1 .. length choices]
    toView' id' inp key x = toView id' key (inp == x) x

-- An input element that allows multiple selections, such as
-- checkboxes or multiple-select boxes.
--
-- When multiple results are submitted, they should all have the same
-- name attribute.
inputChoices :: (Monad m, Functor m, FormInput i f, Monoid v, Eq a)
            => (FormId -> String -> Bool -> a -> v)  -- ^ Choice constructor
            -> [a]                                   -- ^ Default choices
            -> [a]                                   -- ^ Choices
            -> Form m i e v [a]                      -- ^ Resulting form
inputChoices toView defaults choices = Form $ do
    inputKeys <- maybe [] getInputStrings <$> getFormInput
    id' <- getFormId
    formInput <- isFormInput
    let -- Find the actual input, based on the key, or use the default input
        inps = if formInput 
               then mapMaybe (\inputKey -> lookup inputKey $ zip (ids id') choices) inputKeys
               else defaults
        -- Apply the toView' function to all choices
        view' = mconcat $ zipWith (toView' id' inps) (ids id') choices
    return (View (const view'), return (Ok inps))
  where
    ids id' = map (((show id' ++ "-") ++) . show) [1 .. length choices]
    toView' id' inps key x = toView id' key (x `elem` inps) x

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
    (countView, mcountRes) <- unForm $ countField (Just defCount)

    -- We need to evaluate the count
    countRes <- lift $ lift mcountRes
    let count = fromMaybe defCount (getResult countRes)

        -- Use the provided defaults, then loop Nothing
        defaults' = map Just (fromMaybe [] defaults) ++ repeat Nothing

        -- Apply the single form to the defaults
        forms = zipWith ($) (replicate count single) defaults'
    down 2
    list <- mapM (incAfter . unForm) forms
    up 2

    return ( countView `mappend` (mconcat $ map fst list)
           , liftM (combineResults [] []) . sequence $ map snd list
           )
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
