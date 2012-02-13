{-# LANGUAGE ExistentialQuantification #-}
module Text.Digestive.View
    ( View (..)
    , getForm
    , postForm
    ) where

import Data.Text (Text)

import Text.Digestive.Form
import Text.Digestive.Types

data View m v = forall a. View
    { viewForm   :: Form m v a
    , viewInput  :: [(Path, Text)]
    , viewErrors :: [(Path, v)]
    }

getForm :: Form m v a -> View m v
getForm form = View form [] []

postForm :: Monad m => Form m v a -> Env m -> m (Either (View m v) a)
postForm form env = eval env form >>= \(r, inp) -> return $ case r of
    Error errs -> Left $ View form inp errs
    Success x  -> Right x
