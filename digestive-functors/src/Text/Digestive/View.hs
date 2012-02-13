{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings #-}
module Text.Digestive.View
    ( View (..)

      -- * Obtaining a view
    , getForm
    , postForm

      -- * Operations on views
    , subView

      -- * Querying a view

      -- ** Input
    , fieldInputText
    , fieldInputChoice
    , fieldInputBool

      -- ** Errors
    , errors
    , childErrors
    ) where

import Data.List (findIndex, isPrefixOf)
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Digestive.Field
import Text.Digestive.Form
import Text.Digestive.Types

data View m v = forall a. View
    { viewForm   :: Form m v a
    , viewInput  :: [(Path, Text)]
    , viewErrors :: [(Path, v)]
    , viewMethod :: Method
    }

instance Show v => Show (View m v) where
    show (View form input errs method) =
        "View " ++ show form ++ " " ++ show input ++
        " " ++ show errs ++ " " ++ show method

getForm :: Form m v a -> View m v
getForm form = View form [] [] Get

postForm :: Monad m => Form m v a -> Env m -> m (Either (View m v) a)
postForm form env = eval Post env form >>= \(r, inp) -> return $ case r of
    Error errs -> Left $ View form inp errs Post
    Success x  -> Right x

subView :: Text -> View m v -> View m v
subView ref (View form input errs method) = case lookupForm (toPath ref) form of
    (SomeForm f : _) -> View f input' errs' method
    _                -> error $
        "Text.Digestive.View.subView: " ++ T.unpack ref ++ " not found"
  where
    path   = toPath ref
    input' = [(p', i) | (p, i) <- input, p' <- dropPath p]
    errs'  = [(p', e) | (p, e) <- errs, p' <- dropPath p]

    dropPath xs
        | path `isPrefixOf` xs = [drop (length path) xs]
        | otherwise            = []

fieldInputText :: Text -> View m v -> Text
fieldInputText ref (View form input _ method) = fromMaybe "" $
    queryField path form $ \field -> case field of
        Text t -> Just $ evalField method givenInput (Text t)
        _      -> Nothing
  where
    path       = toPath ref
    givenInput = lookup path input

fieldInputChoice :: Text -> View m v -> ([v], Int)
fieldInputChoice ref (View form input _ method) = fromMaybe ([], 0) $
    queryField path form $ \field -> case field of
        Choice xs i -> do
            let x = evalField method givenInput (Choice xs i)
            idx <- findIndex (== x) (map fst xs)
            return (map snd xs, idx)
        _           -> Nothing
  where
    path       = toPath ref
    givenInput = lookup path input

fieldInputBool :: Text -> View m v -> Bool
fieldInputBool ref (View form input _ method) = fromMaybe False $
    queryField path form $ \field -> case field of
        Bool x -> Just $ evalField method givenInput (Bool x)
        _      -> Nothing
  where
    path       = toPath ref
    givenInput = lookup path input

errors :: Text -> View m v -> [v]
errors ref = map snd . filter ((== toPath ref) . fst) . viewErrors

childErrors :: Text -> View m v -> [v]
childErrors ref =
    map snd . filter ((toPath ref `isPrefixOf`) . fst) . viewErrors
