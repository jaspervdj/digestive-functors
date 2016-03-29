--------------------------------------------------------------------------------
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Blaze.Html5
    ( inputText
    , inputTextArea
    , inputPassword
    , inputHidden
    , inputSelect
    , inputRadio
    , inputCheckbox
    , inputFile
    , inputSubmit
    , label
    , form
    , errorList
    , childErrorList
    ) where


--------------------------------------------------------------------------------
import           Control.Monad               (forM_, when)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 (mappend, mempty)
import           Data.Text                   (Text)
import           Text.Blaze.Html             (Html, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as H


--------------------------------------------------------------------------------
import           Text.Digestive.View


--------------------------------------------------------------------------------
(!?) :: H.Attributable h => h -> (Bool, H.Attribute) -> h
(!?) h (False, _) = h
(!?) h (True,  a) = h ! a


--------------------------------------------------------------------------------
inputText :: Text -> View v -> Html
inputText ref view = H.input
    ! A.type_ "text"
    ! A.id    (H.toValue ref')
    ! A.name  (H.toValue ref')
    ! A.value (H.toValue $ fieldInputText ref view)
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
inputTextArea :: Maybe Int  -- ^ Rows
              -> Maybe Int  -- ^ Columns
              -> Text       -- ^ Form path
              -> View Html  -- ^ View
              -> Html       -- ^ Resulting HTML
inputTextArea r c ref view = rows r $ cols c $ H.textarea
    ! A.id     (H.toValue ref')
    ! A.name   (H.toValue ref')
    $ H.toHtml (fieldInputText ref view)
  where
    ref'          = absoluteRef ref view
    rows (Just x) = (! A.rows (H.toValue x))
    rows _        = id
    cols (Just x) = (! A.cols (H.toValue x))
    cols _        = id


--------------------------------------------------------------------------------
inputPassword :: Text -> View v -> Html
inputPassword ref view = H.input
    ! A.type_ "password"
    ! A.id    (H.toValue ref')
    ! A.name  (H.toValue ref')
    ! A.value (H.toValue $ fieldInputText ref view)
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
inputHidden :: Text -> View v -> Html
inputHidden ref view = H.input
    ! A.type_ "hidden"
    ! A.id    (H.toValue ref')
    ! A.name  (H.toValue ref')
    ! A.value (H.toValue $ fieldInputText ref view)
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
inputSelect :: Text -> View Html -> Html
inputSelect ref view = H.select
    ! A.id    (H.toValue ref')
    ! A.name  (H.toValue ref')
    $ forM_ choices $ \(i, c, sel) -> H.option
        !  A.value (value i)
        !? (sel, A.selected "selected")
        $ c
  where
    ref'    = absoluteRef ref view
    value i = H.toValue ref' `mappend` "." `mappend` H.toValue i
    choices = fieldInputChoice ref view


--------------------------------------------------------------------------------
inputRadio :: Bool       -- ^ Add @br@ tags?
           -> Text       -- ^ Form path
           -> View Html  -- ^ View
           -> Html       -- ^ Resulting HTML
inputRadio brs ref view = forM_ choices $ \(i, c, sel) -> do
    let val = value i
    H.input ! A.type_ "radio" ! A.value val ! A.id val ! A.name (H.toValue ref')
        !? (sel, A.checked "checked")
    H.label ! A.for val $ c
    when brs H.br
  where
    ref'    = absoluteRef ref view
    value i = H.toValue ref' `mappend` "." `mappend` H.toValue i
    choices = fieldInputChoice ref view


--------------------------------------------------------------------------------
inputCheckbox :: Text -> View Html -> Html
inputCheckbox ref view = H.input
    !  A.type_ "checkbox"
    !  A.id    (H.toValue ref')
    !  A.name  (H.toValue ref')
    !? (selected, A.checked "checked")
  where
    ref'     = absoluteRef ref view
    selected = fieldInputBool ref view


--------------------------------------------------------------------------------
inputFile :: Text -> View Html -> Html
inputFile ref view = H.input
    ! A.type_ "file"
    ! A.id    (H.toValue ref')
    ! A.name  (H.toValue ref')
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
inputSubmit :: Text -> Html
inputSubmit value = H.input
    ! A.type_ "submit"
    ! A.value (H.toValue value)


--------------------------------------------------------------------------------
label :: Text -> View v -> Html -> Html
label ref view value = H.label
    ! A.for (H.toValue ref')
    $ value
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
form :: View Html -> Text -> Html -> Html
form view action = H.form
    ! A.method  "POST"
    ! A.enctype (H.toValue $ show $ viewEncType view)
    ! A.action  (H.toValue action)


--------------------------------------------------------------------------------
errorList :: Text -> View Html -> Html
errorList ref view = case errors ref view of
    []   -> mempty
    errs -> H.ul ! A.class_ "digestive-functors-error-list" $ forM_ errs $ \e ->
        H.li ! A.class_ "digestive-functors-error" $ e


--------------------------------------------------------------------------------
childErrorList :: Text -> View Html -> Html
childErrorList ref view = case childErrors ref view of
    []   -> mempty
    errs -> H.ul ! A.class_ "digestive-functors-error-list" $ forM_ errs $ \e ->
        H.li ! A.class_ "digestive-functors-error" $ e
