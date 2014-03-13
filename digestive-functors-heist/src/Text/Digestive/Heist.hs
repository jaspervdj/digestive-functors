--------------------------------------------------------------------------------
-- | This module provides a Heist frontend for the digestive-functors library.
--
-- Disclaimer: this documentation requires very basic familiarity with
-- digestive-functors. You might want to take a quick look at this tutorial
-- first:
--
-- <https://github.com/jaspervdj/digestive-functors/blob/master/examples/tutorial.lhs>
--
-- This module exports the functions 'digestiveSplices' and
-- 'bindDigestiveSplices', and most users will not require anything else.
--
-- These splices are used to create HTML for different form elements. This way,
-- the developer doesn't have to care about setting e.g. the previous values in
-- a text field when something goes wrong.
--
-- For documentation on the different splices, see the different functions
-- exported by this module. All splices have the same name as given in
-- 'digestiveSplices'.
--
-- You can give arbitrary attributes to most of the elements (i.e. where it
-- makes sense). This means you can do e.g.:
--
-- > <dfInputTextArea ref="description" cols="20" rows="3" />
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Heist
    ( -- * Core methods
      digestiveSplices
    , bindDigestiveSplices

      -- * Main splices
    , dfInput
    , dfInputList
    , dfInputText
    , dfInputTextArea
    , dfInputPassword
    , dfInputHidden
    , dfInputSelect
    , dfInputSelectGroup
    , dfInputRadio
    , dfInputCheckbox
    , dfInputFile
    , dfInputSubmit
    , dfLabel
    , dfForm
    , dfErrorList
    , dfChildErrorList
    , dfSubView

      -- * Utility splices
    , dfIfChildErrors
    ) where


--------------------------------------------------------------------------------
import           Control.Monad         (liftM, mplus)
import           Control.Monad.Trans
import           Data.Function         (on)
import           Data.List             (unionBy)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           (mappend)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Heist
import           Heist.Interpreted
import qualified Text.XmlHtml          as X


--------------------------------------------------------------------------------
import           Text.Digestive.Form.List
import           Text.Digestive.View


--------------------------------------------------------------------------------
bindDigestiveSplices :: MonadIO m => View Text -> HeistState m -> HeistState m
bindDigestiveSplices = bindSplices . digestiveSplices


--------------------------------------------------------------------------------
digestiveSplices :: MonadIO m => View Text -> Splices (Splice m)
digestiveSplices view = do
    "dfInput"            ## dfInput view
    "dfInputList"        ## dfInputList view
    "dfInputText"        ## dfInputText view
    "dfInputTextArea"    ## dfInputTextArea view
    "dfInputPassword"    ## dfInputPassword view
    "dfInputHidden"      ## dfInputHidden view
    "dfInputSelect"      ## dfInputSelect view
    "dfInputSelectGroup" ## dfInputSelectGroup view
    "dfInputRadio"       ## dfInputRadio view
    "dfInputCheckbox"    ## dfInputCheckbox view
    "dfInputFile"        ## dfInputFile view
    "dfInputSubmit"      ## dfInputSubmit view
    "dfLabel"            ## dfLabel view
    "dfForm"             ## dfForm view
    "dfErrorList"        ## dfErrorList view
    "dfChildErrorList"   ## dfChildErrorList view
    "dfSubView"          ## dfSubView view
    "dfIfChildErrors"    ## dfIfChildErrors view
    


--------------------------------------------------------------------------------
attr :: Bool -> (Text, Text) -> [(Text, Text)] -> [(Text, Text)]
attr False _ = id
attr True  a = (a :)


--------------------------------------------------------------------------------
makeElement :: Text -> [X.Node] -> [(Text, Text)] -> [X.Node]
makeElement name nodes = return . flip (X.Element name) nodes


--------------------------------------------------------------------------------
getRefAttributes :: Monad m
                 => Maybe Text                       -- ^ Optional default ref
                 -> HeistT m m (Text, [(Text, Text)])  -- ^ (Ref, other attrs)
getRefAttributes defaultRef = do
    node <- getParamNode
    return $ case node of
        X.Element _ as _ ->
            let ref = fromMaybe (error $ show node ++ ": missing ref") $
                        lookup "ref" as `mplus` defaultRef
            in (ref, filter ((/= "ref") . fst) as)
        _                -> (error "Wrong type of node!", [])


--------------------------------------------------------------------------------
getContent :: Monad m => HeistT m m [X.Node]
getContent = liftM X.childNodes getParamNode


--------------------------------------------------------------------------------
-- | Does not override existing attributes
addAttrs :: [(Text, Text)]  -- ^ Original attributes
         -> [(Text, Text)]  -- ^ Attributes to add
         -> [(Text, Text)]  -- ^ Resulting attributes
addAttrs = unionBy (on (==) fst)


--------------------------------------------------------------------------------
-- | 
setDisabled :: Text -> View v -> [(Text, Text)] -> [(Text, Text)]
setDisabled ref view = if viewDisabled ref view then (("disabled",""):) else id


--------------------------------------------------------------------------------
-- | Generate an input field with a supplied type. Example:
--
-- > <dfInput type="date" ref="date" />
dfInput :: Monad m => View v -> Splice m
dfInput view = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref'  = absoluteRef ref view
        value = fieldInputText ref view
    return $ makeElement "input" [] $ addAttrs attrs $ setDisabled ref view
        [("id", ref'), ("name", ref'), ("value", value)]


--------------------------------------------------------------------------------
-- | Generate a text input field. Example:
--
-- > <dfInputText ref="user.name" />
dfInputText :: Monad m => View v -> Splice m
dfInputText view = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref'  = absoluteRef ref view
        value = fieldInputText ref view
    return $ makeElement "input" [] $ addAttrs attrs $ setDisabled ref view
        [("type", "text"), ("id", ref'), ("name", ref'), ("value", value)]


--------------------------------------------------------------------------------
-- | Generate a text area. Example:
--
-- > <dfInputTextArea ref="user.about" />
dfInputTextArea :: Monad m => View v -> Splice m
dfInputTextArea view = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref'  = absoluteRef ref view
        value = fieldInputText ref view
    return $ makeElement "textarea" [X.TextNode value] $ addAttrs attrs $
        setDisabled ref view [("id", ref'), ("name", ref')]


--------------------------------------------------------------------------------
-- | Generate a password field. Example:
--
-- > <dfInputPassword ref="user.password" />
dfInputPassword :: Monad m => View v -> Splice m
dfInputPassword view = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref'  = absoluteRef ref view
        value = fieldInputText ref view
    return $ makeElement "input" [] $ addAttrs attrs $ setDisabled ref view
        [("type", "password"), ("id", ref'), ("name", ref'), ("value", value)]


--------------------------------------------------------------------------------
-- | Generate a hidden input field. Example:
--
-- > <dfInputHidden ref="user.forgery" />
dfInputHidden :: Monad m => View v -> Splice m
dfInputHidden view = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref'  = absoluteRef ref view
        value = fieldInputText ref view
    return $ makeElement "input" [] $ addAttrs attrs $ setDisabled ref view
        [("type", "hidden"), ("id", ref'), ("name", ref'), ("value", value)]


--------------------------------------------------------------------------------
-- | Generate a select button (also known as a combo box). Example:
--
-- > <dfInputSelect ref="user.sex" />
dfInputSelect :: Monad m => View Text -> Splice m
dfInputSelect view = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref'     = absoluteRef ref view
        choices  = fieldInputChoice ref view
        kids     = map makeOption choices
        value i  = ref' `mappend` "." `mappend` i

        makeOption (i, c, sel) = X.Element "option"
            (attr sel ("selected", "selected") [("value", value i)])
            [X.TextNode c]

    return $ makeElement "select" kids $ addAttrs attrs $ setDisabled ref view
        [("id", ref'), ("name", ref')]


--------------------------------------------------------------------------------
-- | Generate a select button (also known as a combo box). Example:
--
-- > <dfInputSelectGroup ref="user.sex" />
dfInputSelectGroup :: Monad m => View Text -> Splice m
dfInputSelectGroup view = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref'     = absoluteRef ref view
        choices  = fieldInputChoiceGroup ref view
        kids     = map makeGroup choices
        value i  = ref' `mappend` "." `mappend` i

        makeGroup (name, options) = X.Element "optgroup"
            [("label", name)] $ map makeOption options
        makeOption (i, c, sel) = X.Element "option"
            (attr sel ("selected", "selected") [("value", value i)])
            [X.TextNode c]

    return $ makeElement "select" kids $ addAttrs attrs $ setDisabled ref view
        [("id", ref'), ("name", ref')]


--------------------------------------------------------------------------------
-- | Generate a number of radio buttons. Example:
--
-- > <dfInputRadio ref="user.sex" />
dfInputRadio :: Monad m => View Text -> Splice m
dfInputRadio view = do
    (ref, attrs) <- getRefAttributes Nothing

    let ref'     = absoluteRef ref view
        choices  = fieldInputChoice ref view
        kids     = concatMap makeOption choices
        value i  = ref' `mappend` "." `mappend` i

        makeOption (i, c, sel) =
            [ X.Element "label" [("for", value i)]
              [ X.Element "input"
                  (attr sel ("checked", "checked") $ addAttrs attrs
                      [ ("type", "radio"), ("value", value i)
                      , ("id", value i), ("name", ref')
                      ]) []
              , X.TextNode c]
            ]

    return kids


--------------------------------------------------------------------------------
-- | Generate a checkbox. Example:
--
-- > <dfInputCheckbox ref="user.married" />
dfInputCheckbox :: Monad m => View Text -> Splice m
dfInputCheckbox view = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref'  = absoluteRef ref view
        value = fieldInputBool ref view
    return $ makeElement "input" [] $ addAttrs attrs $
        attr value ("checked", "checked") $ setDisabled ref view
        [("type", "checkbox"), ("id", ref'), ("name", ref')]


--------------------------------------------------------------------------------
-- | Generate a file upload element. Example:
--
-- > <dfInputFile ref="user.avatar" />
dfInputFile :: Monad m => View Text -> Splice m
dfInputFile view = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref'  = absoluteRef ref view
        value = maybe "" T.pack $ fieldInputFile ref view
    return $ makeElement "input" [] $ addAttrs attrs $ setDisabled ref view
        [("type", "file"), ("id", ref'), ("name", ref'), ("value", value)]


--------------------------------------------------------------------------------
-- | Generate a submit button. Example:
--
-- > <dfInputSubmit />
dfInputSubmit :: Monad m => View v -> Splice m
dfInputSubmit _ = do
    (_, attrs) <- getRefAttributes Nothing
    return $ makeElement "input" [] $ addAttrs attrs [("type", "submit")]


--------------------------------------------------------------------------------
-- | Generate a label for a field. Example:
--
-- > <dfLabel ref="user.married">Married: </dfLabel>
-- > <dfInputCheckbox ref="user.married" />
dfLabel :: Monad m => View v -> Splice m
dfLabel view = do
    (ref, attrs) <- getRefAttributes Nothing
    content      <- getContent
    let ref' = absoluteRef ref view
    return $ makeElement "label" content $ addAttrs attrs [("for", ref')]


--------------------------------------------------------------------------------
-- | Generate a form tag with the @method@ attribute set to @POST@ and
-- the @enctype@ set to the right value (depending on the form).
-- Custom @method@ or @enctype@ attributes would override this
-- behavior. Example:
--
-- > <dfForm action="/users/new">
-- >     <dfInputText ... />
-- >     ...
-- >     <dfInputSubmit />
-- > </dfForm>
dfForm :: Monad m => View v -> Splice m
dfForm view = do
    (_, attrs) <- getRefAttributes Nothing
    content    <- getContent
    return $ makeElement "form" content $ addAttrs attrs
        [ ("method", "POST")
        , ("enctype", T.pack (show $ viewEncType view))
        ]


--------------------------------------------------------------------------------
errorList :: [Text] -> [(Text, Text)] -> [X.Node]
errorList []   _     = []
errorList errs attrs = [X.Element "ul" attrs $ map makeError errs]
  where
    makeError e = X.Element "li" [] [X.TextNode e]


--------------------------------------------------------------------------------
-- | Display the list of errors for a certain field. Example:
--
-- > <dfErrorList ref="user.name" />
-- > <dfInputText ref="user.name" />
dfErrorList :: Monad m => View Text -> Splice m
dfErrorList view = do
    (ref, attrs) <- getRefAttributes Nothing
    return $ errorList (errors ref view) attrs


--------------------------------------------------------------------------------
-- | Display the list of errors for a certain form and all forms below it. E.g.,
-- if there is a subform called @\"user\"@:
--
-- > <dfChildErrorList ref="user" />
--
-- Or display /all/ errors for the form:
--
-- > <dfChildErrorList ref="" />
--
-- Which is more conveniently written as:
--
-- > <dfChildErrorList />
dfChildErrorList :: Monad m => View Text -> Splice m
dfChildErrorList view = do
    (ref, attrs) <- getRefAttributes $ Just ""
    return $ errorList (childErrors ref view) attrs


--------------------------------------------------------------------------------
-- | This splice allows reuse of templates by selecting some child of a form
-- tree. While this may sound complicated, it's pretty straightforward and
-- practical. Suppose we have:
--
-- > <dfInputText ref="user.name" />
-- > <dfInputText ref="user.password" />
-- >
-- > <dfInputTextArea ref="comment.body" />
--
-- You may want to abstract the @\"user\"@ parts in some other template so you
-- Don't Repeat Yourself (TM). If you create a template called @\"user-form\"@
-- with the following contents:
--
-- > <dfInputText ref="name" />
-- > <dfInputText ref="password" />
--
-- You will be able to use:
--
-- > <dfSubView ref="user">
-- >     <apply template="user-form" />
-- > </dfSubView>
-- >
-- > <dfInputTextArea ref="comment.body" />
dfSubView :: MonadIO m => View Text -> Splice m
dfSubView view = do
    (ref, _) <- getRefAttributes Nothing
    let view' = subView ref view
    nodes <- localHS (bindDigestiveSplices view') runChildren
    return nodes


disableOnclick :: Text -> View v -> [(Text, Text)] -> [(Text, Text)]
disableOnclick ref view =
    if viewDisabled ref view then const [("disabled","")] else id


--------------------------------------------------------------------------------
-- | This splice allows variable length lists.  It binds several attribute
-- splices providing functionality for dynamically manipulating the list.  The
-- following descriptions will use the example of a form named \"foo\" with a
-- list subform named \"items\".
--
-- Splices:
--   dfListItem - This tag must surround the markup for a single list item.
--     It surrounds all of its children with a div with id \"foo.items\" and
--     class \"inputList\".
-- 
-- Attribute Splices:
--   itemAttrs - Attribute you should use on div, span, etc that surrounds all
--     the markup for a single list item.  This splice expands to an id of
--     \"foo.items.ix\" (where ix is the index of the current item) and a
--     class of \"inputListItem\".
--   addControl - Use this attribute on the tag you use to define a control
--     for adding elements to the list (usually a button or anchor).  It adds
--     an onclick attribute that calls a javascript function addInputListItem.
--   removeControl - Use this attribute on the control for removing individual
--     items.  It adds an onclick attribute that calls removeInputListItem.
dfInputList :: MonadIO m => View Text -> Splice m
dfInputList view = do
    (ref, _) <- getRefAttributes Nothing
    let listRef = absoluteRef ref view
        listAttrs =
            [ ("id", listRef)
            , ("class", "inputList")
            ]
        addControl _ = return $ disableOnclick ref view
            [ ("onclick", T.concat [ "addInputListItem(this, '"
                                   , listRef
                                   , "'); return false;"] ) ]
        removeControl _ = return $ disableOnclick ref view
            [ ("onclick", T.concat [ "removeInputListItem(this, '"
                                   , listRef
                                   , "'); return false;"] ) ]
        itemAttrs v _ = return
            [ ("id", T.concat [listRef, ".", last $ "0" : viewContext v])
            , ("class", T.append listRef ".inputListItem")
            ]
        templateAttrs v _ = return
            [ ("id", T.concat [listRef, ".", last $ "-1" : viewContext v])
            , ("class", T.append listRef ".inputListTemplate")
            , ("style", "display: none;")
            ]
        items = listSubViews ref view
        f attrs v = localHS (bindAttributeSplices ("itemAttrs" ## attrs v) .
                       bindDigestiveSplices v) runChildren
        dfListItem = do
            template <- f templateAttrs (makeListSubView ref (-1) view)
            res <- mapSplices (f itemAttrs) items
            return $ template ++ res
        attrSplices = do
            "addControl"    ## addControl
            "removeControl" ## removeControl
    nodes <- localHS (bindSplices ("dfListItem" ## dfListItem) .
                      bindAttributeSplices attrSplices) runChildren
    let indices = [X.Element "input"
                    [ ("type", "hidden")
                    , ("name", T.intercalate "." [listRef, indicesRef])
                    , ("value", T.intercalate "," $ map
                        (last . ("0":) . viewContext) items)
                    ] []
                  ]
    return [X.Element "div" listAttrs (indices ++ nodes)]


--------------------------------------------------------------------------------
-- | Render some content only if there are any errors. This is useful for markup
-- purposes.
--
-- > <dfIfChildErrors ref="user">
-- >     Content to be rendered if there are any errors...
-- > </dfIfChildErrors>
--
-- The @ref@ attribute can be omitted if you want to check the entire form.
dfIfChildErrors :: Monad m => View v -> Splice m
dfIfChildErrors view = do
    (ref, _) <- getRefAttributes $ Just ""
    if null (childErrors ref view)
        then return []
        else runChildren
