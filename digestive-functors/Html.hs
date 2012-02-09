{-# LANGUAGE OverloadedStrings #-}
module Html where

import Control.Monad (forM_, mplus)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)

import Data.Text (Text)
import Text.Blaze (Html, (!))
import Text.Blaze.Renderer.Pretty (renderHtml)
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Field

--------------------------------------------------------------------------------

fieldTextInput :: Text -> View v a -> Text
fieldTextInput ref view = fromMaybe "" $ mplus givenInput defaultInput
  where
    path         = toPath ref
    givenInput   = lookup path $ viewInput view
    defaultInput = queryField path (viewForm view) $ fieldDefaultInput

fieldChoiceInput :: Text -> View v a -> ([v], Int)
fieldChoiceInput ref view = fromMaybe ([], 0) $ do
    (choices, idx) <- defaultInput
    return (choices, fromMaybe idx givenInput)
  where
    path         = toPath ref
    givenInput   = lookup path (viewInput view) >>= readMaybe . T.unpack
    defaultInput = queryField path (viewForm view) $ \field -> case field of
        Choice xs i -> Just (map snd xs, i)        
        _           -> Nothing

errors :: Text -> View v a -> [v]
errors ref = map snd . filter ((== toPath ref) . fst) . viewErrors

childErrors :: Text -> View v a -> [v]
childErrors ref =
    map snd . filter ((toPath ref `isPrefixOf`) . fst) . viewErrors

--------------------------------------------------------------------------------

errorList :: Text -> View Html a -> Html
errorList ref view = H.ul $ mapM_ H.li $ errors ref view

--------------------------------------------------------------------------------

label :: Text -> Html -> Html
label ref value = H.label
    ! A.for (H.toValue ref)
    $ value

inputText :: Text -> View v a -> Html
inputText ref view = H.input
    ! A.type_ "text"
    ! A.id    (H.toValue ref)
    ! A.name  (H.toValue ref)
    ! A.value (H.toValue $ fieldTextInput ref view)

inputSelect :: Text -> View Html a -> Html
inputSelect ref view = H.select
    ! A.id    (H.toValue ref)
    ! A.name  (H.toValue ref)
    $ forM_ (zip choices [0 ..]) $ \(choice, i) -> select i $
        H.option ! A.value (H.toValue i) $ choice
  where
    (choices, idx)  = fieldChoiceInput ref view
    select i
        | i == idx  = (! A.selected "selected")
        | otherwise = id

inputSubmit :: Text -> Html
inputSubmit value = H.input
    ! A.type_ "submit"
    ! A.value (H.toValue value)

--------------------------------------------------------------------------------

userView :: View Html a -> Html
userView v = do
    label "name" "Name: "
    inputText "name" v
    errorList "name" v

    label "age" "Age: "
    inputText "age" v
    errorList "age" v

    label "sex" "Sex: "
    inputSelect "sex" v
    errorList "sex" v

    inputSubmit "Submit"
