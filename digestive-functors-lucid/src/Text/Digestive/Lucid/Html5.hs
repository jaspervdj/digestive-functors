--------------------------------------------------------------------------------
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Text.Digestive.Lucid.Html5
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
import           Data.Text                   (Text, pack)
import           Lucid
import           Lucid.Base

--------------------------------------------------------------------------------
import           Text.Digestive.View


--------------------------------------------------------------------------------
ifSingleton :: Bool -> a -> [a]
ifSingleton False _ = []
ifSingleton True  a = [a]

--------------------------------------------------------------------------------
inputText :: Text -> View v -> Html ()
inputText ref view = input_
    [ type_    "text"
    , id_      ref'
    , name_    ref'
    , value_ $ fieldInputText ref view
    ]
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
inputTextArea :: Maybe Int      -- ^ Rows
              -> Maybe Int      -- ^ Columns
              -> Text           -- ^ Form path
              -> View (Html ()) -- ^ View
              -> Html ()        -- ^ Resulting HTML
inputTextArea r c ref view = textarea_
    ([ id_     ref'
     , name_   ref'
     ] ++ (rows' r) ++ (cols' c)) $
        toHtmlRaw $ fieldInputText ref view
  where
    ref'          = absoluteRef ref view
    rows' (Just x) = [rows_ $ pack $ show x]
    rows' _        = []
    cols' (Just x) = [cols_ $ pack $ show x]
    cols' _        = []


--------------------------------------------------------------------------------
inputPassword :: Text -> View v -> Html ()
inputPassword ref view = input_
    [ type_    "password"
    , id_      ref'
    , name_    ref'
    , value_ $ fieldInputText ref view
    ]
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
inputHidden :: Text -> View v -> Html ()
inputHidden ref view = input_
    [ type_    "hidden"
    , id_      ref'
    , name_    ref'
    , value_ $ fieldInputText ref view
    ]
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
inputSelect :: Text -> View (Html ()) -> Html ()
inputSelect ref view = select_
    [ id_   ref'
    , name_ ref'
    ] $ forM_ choices $ \(i, c, sel) -> option_
          ([value_ (value i)] ++ (ifSingleton sel $ selected_ "selected")) c
  where
    ref'    = absoluteRef ref view
    value i = ref' `mappend` "." `mappend` i
    choices = fieldInputChoice ref view


--------------------------------------------------------------------------------
inputRadio :: Bool           -- ^ Add @br@ tags?
           -> Text           -- ^ Form path
           -> View (Html ()) -- ^ View
           -> Html ()        -- ^ Resulting HTML
inputRadio brs ref view = forM_ choices $ \(i, c, sel) -> do
    let val = value i
    input_ $ [type_ "radio", value_ val, id_ val, name_ ref']
               ++ (ifSingleton sel checked_)
    label_ [for_ val] c
    when brs (br_ [])
  where
    ref'    = absoluteRef ref view
    value i = ref' `mappend` "." `mappend` i
    choices = fieldInputChoice ref view


--------------------------------------------------------------------------------
inputCheckbox :: Text -> View (Html ()) -> Html ()
inputCheckbox ref view = input_ $
    [ type_ "checkbox"
    , id_   ref'
    , name_ ref'
    ] ++ (ifSingleton selected checked_)
  where
    ref'     = absoluteRef ref view
    selected = fieldInputBool ref view


--------------------------------------------------------------------------------
inputFile :: Text -> View (Html ()) -> Html ()
inputFile ref view = input_
    [ type_  "file"
    , id_    ref'
    , name_  ref'
    , value_ $ pack value
    ]
  where
    ref'  = absoluteRef ref view
    value = fromMaybe "" $ fieldInputFile ref view


--------------------------------------------------------------------------------
inputSubmit :: Text -> Html ()
inputSubmit value = input_
    [ type_  "submit"
    , value_ value
    ]


--------------------------------------------------------------------------------
label :: Text -> View v -> Html () -> Html ()
label ref view value = label_
    [ for_ ref'
    ] $ value
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
form :: View (Html ()) -> Text -> Html () -> Html ()
form view action = form_
    [ method_  "POST"
    , enctype_ (pack $ show $ viewEncType view)
    , action_  action
    ]


--------------------------------------------------------------------------------
errorList :: Text -> View (Html ()) -> Html ()
errorList ref view = case errors ref view of
    []   -> mempty
    errs -> ul_ [class_ "digestive-functors-error-list"] $ forM_ errs $ \e ->
              li_ [class_ "digestive-functors-error"] e


--------------------------------------------------------------------------------
childErrorList :: Text -> View (Html ()) -> Html ()
childErrorList ref view = case childErrors ref view of
    []   -> mempty
    errs -> ul_ [class_ "digestive-functors-error-list"] $ forM_ errs $ \e ->
              li_ [class_ "digestive-functors-error"] e
