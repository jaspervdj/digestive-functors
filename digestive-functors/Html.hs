{-# LANGUAGE OverloadedStrings #-}
module Html where

import Control.Monad (mplus)
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import Text.Blaze (Html, (!))
import Text.Blaze.Renderer.Pretty (renderHtml)
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Field

--------------------------------------------------------------------------------

fieldInput :: Text -> View v a -> Text
fieldInput ref view = fromMaybe "" $ mplus givenInput defaultInput
  where
    path         = toPath ref
    givenInput   = lookup path $ viewInput view
    defaultInput = case lookupForm path (viewForm view) of
        (SomeForm f : _) -> formDefaultInput f

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
    ! A.value (H.toValue $ fieldInput ref view)

inputSubmit :: Text -> Html
inputSubmit value = H.input
    ! A.type_ "submit"
    ! A.value (H.toValue value)

--------------------------------------------------------------------------------

userView :: View v a -> Html
userView v = do
    label "name" "Name: "
    inputText "name" v

    label "age" "Age: "
    inputText "age" v

    label "sex" "Sex: "
    inputText "sex" v

    inputSubmit "Submit"
