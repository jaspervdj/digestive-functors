{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings #-}
module Text.Digestive.View
    ( View (..)
    , getForm
    , postForm

      -- * Querying fields of a view
    , fieldTextInput
    , fieldChoiceInput
    ) where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

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
    show (View form input errors method) =
        "View " ++ show form ++ " " ++ show input ++
        " " ++ show errors ++ " " ++ show method

getForm :: Form m v a -> View m v
getForm form = View form [] [] Get

postForm :: Monad m => Form m v a -> Env m -> m (Either (View m v) a)
postForm form env = eval Post env form >>= \(r, inp) -> return $ case r of
    Error errs -> Left $ View form inp errs Post
    Success x  -> Right x

fieldTextInput :: Text -> View m v -> Text
fieldTextInput ref (View form input _ method) = fromMaybe "" $
    queryField path form $ \field -> case field of
        Text t -> Just $ evalField method givenInput (Text t)
        _      -> Nothing
  where
    path       = toPath ref
    givenInput = lookup path input

fieldChoiceInput :: Text -> View m v -> ([v], Int)
fieldChoiceInput ref (View form input _ method) = fromMaybe ([], 0) $
    queryField path form $ \field -> case field of
        Choice xs i -> do
            let x = evalField method givenInput (Choice xs i)
            idx <- findIndex (== x) (map fst xs)
            return (map snd xs, idx)
        _           -> Nothing
  where
    path       = toPath ref
    givenInput = lookup path input
