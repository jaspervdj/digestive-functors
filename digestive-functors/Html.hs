{-# LANGUAGE OverloadedStrings #-}
module Html where

import Data.Monoid (mappend)
import Control.Monad (forM_, mplus, when)
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

fieldTextInput :: Text -> View m v a -> Text
fieldTextInput ref view = fromMaybe "" $ mplus givenInput defaultInput
  where
    path         = toPath ref
    givenInput   = lookup path $ viewInput view
    defaultInput = queryField path (viewForm view) $ fieldDefaultInput

fieldChoiceInput :: Text -> View m v a -> ([v], Int)
fieldChoiceInput ref view = fromMaybe ([], 0) $ do
    (choices, idx) <- defaultInput
    return (choices, fromMaybe idx givenInput)
  where
    path         = toPath ref
    givenInput   = lookup path (viewInput view) >>= readMaybe . T.unpack
    defaultInput = queryField path (viewForm view) $ \field -> case field of
        Choice xs i -> Just (map snd xs, i)        
        _           -> Nothing

errors :: Text -> View m v a -> [v]
errors ref = map snd . filter ((== toPath ref) . fst) . viewErrors

childErrors :: Text -> View m v a -> [v]
childErrors ref =
    map snd . filter ((toPath ref `isPrefixOf`) . fst) . viewErrors

--------------------------------------------------------------------------------

errorList :: Text -> View m Html a -> Html
errorList ref view = H.ul $ mapM_ H.li $ errors ref view

--------------------------------------------------------------------------------

label :: Text -> Html -> Html
label ref value = H.label
    ! A.for (H.toValue ref)
    $ value

inputText :: Text -> View m v a -> Html
inputText ref view = H.input
    ! A.type_ "text"
    ! A.id    (H.toValue ref)
    ! A.name  (H.toValue ref)
    ! A.value (H.toValue $ fieldTextInput ref view)

inputSelect :: Text -> View m Html a -> Html
inputSelect ref view = H.select
    ! A.id    (H.toValue ref)
    ! A.name  (H.toValue ref)
    $ forM_ (zip choices [0 ..]) $ \(c, i) -> select i $
        H.option ! A.value (value i) $ c
  where
    value i         = H.toValue ref `mappend` "." `mappend` H.toValue i
    (choices, idx)  = fieldChoiceInput ref view
    select i
        | i == idx  = (! A.selected "selected")
        | otherwise = id

inputRadio :: Bool           -- ^ Add @br@ tags?
           -> Text           -- ^ Form path
           -> View m Html a  -- ^ View
           -> Html           -- ^ Resulting HTML
inputRadio brs ref view = forM_ (zip choices [0 ..]) $ \(c, i) -> do
    let val = value i
    select i $ H.input ! A.type_ "radio" ! A.value val
        ! A.id val ! A.name (H.toValue ref)
    H.label ! A.for val $ c
    when brs H.br
  where
    value i         = H.toValue ref `mappend` "." `mappend` H.toValue i
    (choices, idx)  = fieldChoiceInput ref view
    select i
        | i == idx  = (! A.checked "checked")
        | otherwise = id

inputSubmit :: Text -> Html
inputSubmit value = H.input
    ! A.type_ "submit"
    ! A.value (H.toValue value)

form :: Text -> Html -> Html
form action = H.form ! A.method "POST" ! A.action (H.toValue action)
