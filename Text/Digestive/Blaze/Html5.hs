{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Blaze.Html5 where

import Control.Applicative ((<$>))
import Control.Monad (mplus, forM_, unless, when)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Digestive.Types
import qualified Text.Digestive.Common as Common

-- | Checks the input element when the argument is true
--
checked :: Bool -> Html -> Html
checked False x = x
checked True  x = x ! A.checked "checked"

inputText :: (Monad m, Functor m)
          => Maybe String
          -> Form m String e Html String
inputText = Common.inputString $ \id' inp ->
    H.input ! A.type_ "text"
            ! A.name (H.stringValue $ show id')
            ! A.id (H.stringValue $ show id')
            ! A.value (H.stringValue $ fromMaybe "" inp)

inputTextArea :: (Monad m, Functor m)
              => Maybe Int                    -- ^ Rows
              -> Maybe Int                    -- ^ Columns
              -> Maybe String                 -- ^ Default input
              -> Form m String e Html String  -- ^ Result
inputTextArea r c = Common.inputString $ \id' inp -> rows r $ cols c $
    H.textarea ! A.name (H.stringValue $ show id')
               ! A.id (H.stringValue $ show id')
               $ H.string $ fromMaybe "" inp
  where
    rows Nothing = id
    rows (Just x) = (! A.rows (H.stringValue $ show x))
    cols Nothing = id
    cols (Just x) = (! A.cols (H.stringValue $ show x))

inputTextRead :: (Monad m, Functor m, Show a, Read a)
              => String
              -> Maybe a
              -> Form m String String Html a
inputTextRead error' = flip Common.inputRead error' $ \id' inp ->
    H.input ! A.type_ "text"
            ! A.name (H.stringValue $ show id')
            ! A.id (H.stringValue $ show id')
            ! A.value (H.stringValue $ fromMaybe "" inp)

inputPassword :: (Monad m, Functor m)
              => Form m String e Html String
inputPassword = flip Common.inputString Nothing $ \id' inp ->
    H.input ! A.type_ "password"
            ! A.name (H.stringValue $ show id')
            ! A.id (H.stringValue $ show id')
            ! A.value (H.stringValue $ fromMaybe "" inp)

inputCheckBox :: (Monad m, Functor m)
              => Bool
              -> Form m String e Html Bool
inputCheckBox inp = flip Common.inputBool inp $ \id' inp ->
    checked inp $ H.input ! A.type_ "checkbox"
                          ! A.name (H.stringValue $ show id')
                          ! A.id (H.stringValue $ show id')

inputRadio :: (Monad m, Functor m, Eq a)
           => Bool                        -- ^ Use @<br>@ tags
           -> a                           -- ^ Default option
           -> [(a, Html)]                 -- ^ Choices with their names
           -> Form m String e Html a      -- ^ Resulting form
inputRadio br def choices = Common.inputChoice toView def (map fst choices)
  where
    toView group id' sel val = do
        checked sel $ H.input ! A.type_ "radio"
                              ! A.name (H.stringValue $ show group)
                              ! A.value (H.stringValue id')
                              ! A.id (H.stringValue id')
        H.label ! A.for (H.stringValue id')
                $ fromMaybe mempty $ lookup val choices
        when br H.br

label :: Monad m
      => String
      -> Form m i e Html ()
label string = Common.label $ \id' ->
    H.label ! A.for (H.stringValue $ show id')
            $ H.string string

errorList :: [String] -> Html
errorList errors = unless (null errors) $
    H.ul $ forM_ errors $ H.li . H.string

errors :: Monad m
       => Form m i String Html ()
errors = Common.errors errorList

childErrors :: Monad m
            => Form m i String Html ()
childErrors = Common.childErrors errorList
