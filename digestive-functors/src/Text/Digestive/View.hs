{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings #-}
module Text.Digestive.View
    ( View (..)

      -- * Obtaining a view
    , getForm
    , postForm

      -- * Operations on views
    , subView

      -- * Querying a view
      -- ** Low-level
    , absolutePath

      -- ** Form encoding
    , viewEncType

      -- ** Input
    , fieldInputText
    , fieldInputChoice
    , fieldInputBool
    , fieldInputFile

      -- ** Errors
    , errors
    , childErrors
    ) where

import Control.Arrow (second)
import Data.List (findIndex, isPrefixOf)
import Data.Maybe (fromMaybe)

import Data.Text (Text)

import Text.Digestive.Field
import Text.Digestive.Form.Encoding
import Text.Digestive.Form.Internal
import Text.Digestive.Types

data View v = forall a m. Monad m => View
    { viewContext :: Path
    , viewForm    :: Form v m a
    , viewInput   :: [(Path, FormInput)]
    , viewErrors  :: [(Path, v)]
    , viewMethod  :: Method
    }

instance Functor View where
    fmap f (View ctx form input errs method) = View
        ctx (formMapView f form) input (map (second f) errs) method

instance Show v => Show (View v) where
    show (View ctx form input errs method) =
        "View " ++ show ctx ++ " " ++ show form ++ " " ++ show input ++
        " " ++ show errs ++ " " ++ show method

getForm :: Monad m => Form v m a -> View v
getForm form = View [] form [] [] Get

postForm :: Monad m => Form v m a -> Env m -> m (View v, Maybe a)
postForm form env = eval Post env form >>= \(r, inp) -> return $ case r of
    Error errs -> (View [] form inp errs Post, Nothing)
    Success x  -> (View [] form inp [] Post, Just x)

subView :: Text -> View v -> View v
subView ref (View ctx form input errs method) =
    View (ctx ++ path) form input errs method
  where
    path = toPath ref

-- | Determine an absolute 'Path' for a field in the form
absolutePath :: Text -> View v -> Path
absolutePath ref (View ctx _ _ _ _) = ctx ++ toPath ref

viewEncType :: View v -> FormEncType
viewEncType (View _ form _ _ _) = formEncType form

lookupInput :: Path -> [(Path, FormInput)] -> [FormInput]
lookupInput path = map snd . filter ((== path) . fst)

fieldInputText :: Text -> View v -> Text
fieldInputText ref view@(View _ form input _ method) =
    queryField path form $ \field -> case field of
        Text t -> evalField method givenInput (Text t)
        _      -> ""  -- TODO: perhaps throw error?
  where
    path       = absolutePath ref view
    givenInput = lookupInput path input

fieldInputChoice :: Text -> View v -> ([v], Int)
fieldInputChoice ref view@(View _ form input _ method) =
    queryField path form $ \field -> case field of
        Choice xs i ->
            let x   = evalField method givenInput (Choice xs i)
                idx = fromMaybe 0 $ findIndex (== x) (map fst xs)
            in (map snd xs, idx)
        _           -> ([], 0)  -- TODO: perhaps throw error?
  where
    path       = absolutePath ref view
    givenInput = lookupInput path input

fieldInputBool :: Text -> View v -> Bool
fieldInputBool ref view@(View _ form input _ method) =
    queryField path form $ \field -> case field of
        Bool x -> evalField method givenInput (Bool x)
        _      -> False  -- TODO: perhaps throw error?
  where
    path       = absolutePath ref view
    givenInput = lookupInput path input

fieldInputFile :: Text -> View v -> Maybe FilePath
fieldInputFile ref view@(View _ form input _ method) =
    queryField path form $ \field -> case field of
        File -> evalField method givenInput File
        _    -> Nothing  -- TODO: perhaps throw error?
  where
    path       = absolutePath ref view
    givenInput = lookupInput path input

errors :: Text -> View v -> [v]
errors ref view = map snd $ filter ((== absolutePath ref view) . fst) $
    viewErrors view

childErrors :: Text -> View v -> [v]
childErrors ref view = map snd $
    filter ((absolutePath ref view `isPrefixOf`) . fst) $ viewErrors view
