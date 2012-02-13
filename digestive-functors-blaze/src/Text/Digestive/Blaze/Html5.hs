{-# LANGUAGE GADTs, OverloadedStrings #-}
module Text.Digestive.Blaze.Html5 where

import Data.Monoid (mappend)
import Control.Monad (forM_, when)

import Data.Text (Text)
import Text.Blaze.Internal as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Digestive.View

errorList :: Text -> View m Html -> Html
errorList ref view = H.ul $ mapM_ H.li $ errors ref view

--------------------------------------------------------------------------------

(!?) :: H.Attributable h => h -> (Bool, H.Attribute) -> h
(!?) h (False, _) = h
(!?) h (True,  a) = h ! a

label :: Text -> Html -> Html
label ref value = H.label ! A.for (H.toValue ref) $ value

inputText :: Text -> View m v -> Html
inputText ref view = H.input
    ! A.type_ "text"
    ! A.id    (H.toValue ref)
    ! A.name  (H.toValue ref)
    ! A.value (H.toValue $ fieldTextInput ref view)

inputSelect :: Text -> View m Html -> Html
inputSelect ref view = H.select
    ! A.id    (H.toValue ref)
    ! A.name  (H.toValue ref)
    $ forM_ (zip choices [0 ..]) $ \(c, i) -> H.option
        !  A.value (value i)
        !? (i == idx, A.selected "selected")
        $ c
  where
    value i         = H.toValue ref `mappend` "." `mappend` H.toValue i
    (choices, idx)  = fieldChoiceInput ref view

inputRadio :: Bool         -- ^ Add @br@ tags?
           -> Text         -- ^ Form path
           -> View m Html  -- ^ View
           -> Html         -- ^ Resulting HTML
inputRadio brs ref view = forM_ (zip choices [0 ..]) $ \(c, i) -> do
    let val = value i
    H.input ! A.type_ "radio" ! A.value val ! A.id val ! A.name (H.toValue ref)
        !? (i == idx, A.checked "checked")
    H.label ! A.for val $ c
    when brs H.br
  where
    value i        = H.toValue ref `mappend` "." `mappend` H.toValue i
    (choices, idx) = fieldChoiceInput ref view

inputCheckbox :: Text -> View m Html -> Html
inputCheckbox ref view = H.input
    !  A.type_ "checkbox"
    !  A.id    (H.toValue ref)
    !  A.name  (H.toValue ref)
    !? (selected, A.checked "checked")
  where
    selected = fieldBoolInput ref view

inputSubmit :: Text -> Html
inputSubmit value = H.input
    ! A.type_ "submit"
    ! A.value (H.toValue value)

form :: Text -> Html -> Html
form action = H.form ! A.method "POST" ! A.action (H.toValue action)
