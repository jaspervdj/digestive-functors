{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Blaze.Html5 where

import Control.Applicative ((<$>))
import Control.Monad (mplus, forM_, unless)
import Data.Maybe (fromMaybe)

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Digestive.Types
import qualified Text.Digestive.Common as Common

inputString :: (Monad m, Functor m)
            => Maybe String
            -> Form m String e Html String
inputString defaultInput = Common.inputString defaultInput $ \id' inp ->
    H.input ! A.type_ "text"
            ! A.name (H.stringValue $ show id')
            ! A.id (H.stringValue $ show id')
            ! A.value (H.stringValue $ fromMaybe "" inp)

inputRead :: (Monad m, Functor m, Show a, Read a)
          => Maybe a
          -> Form m String String Html a
inputRead defaultInput =
    Common.inputRead "No read" defaultInput $ \id' inp ->
        H.input ! A.type_ "text"
                ! A.name (H.stringValue $ show id')
                ! A.id (H.stringValue $ show id')
                ! A.value (H.stringValue $ fromMaybe "" inp)

inputPassword :: (Monad m, Functor m)
              => Form m String e Html String
inputPassword = Common.inputString Nothing $ \id' inp ->
    H.input ! A.type_ "password"
            ! A.name (H.stringValue $ show id')
            ! A.id (H.stringValue $ show id')
            ! A.value (H.stringValue $ fromMaybe "" inp)

inputBool :: (Monad m, Functor m)
          => Bool
          -> Form m String String Html Bool
inputBool defaultInput = Common.inputBool defaultInput $ \id' inp ->
    checked inp $ H.input ! A.type_ "checkbox"
                          ! A.name (H.stringValue $ show id')
                          ! A.id (H.stringValue $ show id')
  where
    checked False x = x
    checked True  x = x ! A.checked "checked"

prependLabel :: Monad m
             => Html
             -> Form m i e Html a
             -> Form m i e Html a
prependLabel inner = mapViewWithId $ \id' html -> do
    H.label ! A.for (H.stringValue $ show id')
            $ inner
    html

appendErrors :: Monad m
             => Form m i String Html a
             -> Form m i String Html a
appendErrors = mapViewWithErrors $ \errors html -> do
    html
    unless (null errors) $
        H.ul $ forM_ errors $ \e ->
            H.li $ H.string e

appendChildErrors :: Monad m
                  => Form m i String Html a
                  -> Form m i String Html a
appendChildErrors = mapViewWithChildErrors $ \errors html -> do
    html
    unless (null errors) $
        H.ul $ forM_ errors $ \e ->
            H.li $ H.string e
