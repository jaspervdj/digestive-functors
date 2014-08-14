--------------------------------------------------------------------------------
-- | This module provides a compiled Heist frontend for the digestive-functors
-- library.
--
-- Disclaimer: this documentation requires very basic familiarity with
-- digestive-functors. You might want to take a quick look at this tutorial
-- first:
--
-- <https://github.com/jaspervdj/digestive-functors/blob/master/examples/tutorial.lhs>
--
-- This module exports the 'formSplice' function, and most users will not
-- require anything else.
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
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Text.Digestive.Heist.Compiled
    ( -- * Core methods
      formSplice
    , formSplice'

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
    , dfErrorList
    , dfChildErrorList
    , dfSubView

      -- * Utility splices
    , dfIfChildErrors
    , digestiveSplices
    ) where


--------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Monad            (mplus)
import           Control.Monad.Trans      (MonadIO, liftIO)
import           Data.Function            (on)
import           Data.List                (unionBy)
import           Data.Monoid              (mappend, mconcat, mempty, (<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding
import           Heist
import           Heist.Compiled
import           Heist.Compiled.LowLevel
import           Text.Printf
import qualified Text.XmlHtml             as X
------------------------------------------------------------------------------
import           Text.Digestive.Form.List
import           Text.Digestive.View


------------------------------------------------------------------------------
-- | List of splices defined for forms.  For most uses the formSplice function
-- will be fine and you won't need to use this directly.  But this is
-- available if you need more customization.
digestiveSplices :: (MonadIO m)
                 => RuntimeSplice m (View Text)
                 -> Splices (Splice m)
digestiveSplices vp = do
    "dfInput"            ## dfInput vp
    "dfValue"            ## dfValue vp
    "dfInputText"        ## dfInputText vp
    "dfInputTextArea"    ## dfInputTextArea vp
    "dfInputPassword"    ## dfInputPassword vp
    "dfInputHidden"      ## dfInputHidden vp
    "dfInputSelect"      ## dfInputSelect vp
    "dfInputSelectGroup" ## dfInputSelectGroup vp
    "dfInputRadio"       ## dfInputRadio vp
    "dfInputCheckbox"    ## dfInputCheckbox vp
    "dfInputFile"        ## dfInputFile vp
    "dfInputSubmit"      ## dfInputSubmit
    "dfLabel"            ## dfLabel vp
    "dfErrorList"        ## dfErrorList vp
    "dfChildErrorList"   ## dfChildErrorList vp
    "dfSubView"          ## dfSubView vp
    "dfIfChildErrors"    ## dfIfChildErrors vp
    "dfIfNoChildErrors"  ## dfIfNoChildErrors vp
    "dfInputList"        ## dfInputList vp
    "dfEncType"          ## dfEncType vp


------------------------------------------------------------------------------
-- | A compiled splice for a specific form.  You pass in a runtime action that
-- gets the form's view and this function returns a splice that creates a form
-- tag.  In your HeistConfig you might have a compiled splice like this:
--
-- > ("customerForm" ## formSplice mempty mempty
-- >                    (liftM fst $ runForm "customer" custForm))
--
-- Then you can use the customerForm tag just like you would use the dfForm
-- tag in interpreted templates anywhere you want to have a customer form.
formSplice :: MonadIO m
           => Splices (Splice m)
             -- ^ Extra splices that you want to have available inside the
             -- form tag.
           -> Splices (AttrSplice m)
             -- ^ Attribute splices available inside the form tag.
           -> RuntimeSplice m (View Text)
           -> Splice m
formSplice ss as = formSplice' (const ss) (const as)


------------------------------------------------------------------------------
-- | Same as 'formSplice' except the supplied splices and attribute
-- splices are applied to the resulting form view.
formSplice' :: MonadIO m
            => (RuntimeSplice m (View Text) -> Splices (Splice m))
            -> (RuntimeSplice m (View Text) -> Splices (AttrSplice m))
            -> RuntimeSplice m (View Text)
            -> Splice m
formSplice' ss as = deferMap return $ \getView -> do
    node <- getParamNode
    (_, attrs) <- getRefAttributes node (Just "")
    let tree = X.Element "form"
                 (addAttrs attrs
                    [ ("method", "POST")
                    , ("enctype", "${dfEncType}")
                    ])
                 (X.childNodes node)
        action = runNode tree
    withLocalSplices (digestiveSplices getView `mappend` ss getView) (as getView) action


--------------------------------------------------------------------------------
attr :: Bool -> (Text, Text) -> [(Text, Text)] -> [(Text, Text)]
attr False _ = id
attr True  a = (a :)


--------------------------------------------------------------------------------
-- |
setDisabled :: Text -> View v -> [(Text, Text)] -> [(Text, Text)]
setDisabled ref view = if viewDisabled ref view then (("disabled",""):) else id


--------------------------------------------------------------------------------
getRefAttributes
    :: MonadIO m
    => X.Node
    -> Maybe Text              -- ^ Optional default ref
    -> HeistT n m (Text, [(Text, Text)])
getRefAttributes node defaultRef = do
    tfp <- getTemplateFilePath
    getRefAttributes' tfp node defaultRef


--------------------------------------------------------------------------------
getRefAttributes'
    :: MonadIO m
    => Maybe FilePath
    -> X.Node
    -> Maybe Text              -- ^ Optional default ref
    -> m (Text, [(Text, Text)])
getRefAttributes' tfp node defaultRef = do
    let end s = do
            liftIO $ putStrLn s
            return ("", [])
    case node of
        X.Element n as _ -> do
            case lookup "ref" as `mplus` defaultRef of
              Nothing -> end $ printf "%s: missing ref, path %s"
                               (T.unpack n) (show tfp)
              Just ref -> return (ref, filter ((/= "ref") . fst) as)
        _                ->
            end $ "Wrong type of node! (" ++ show node ++ ")"


dfEncType :: (Monad m)
          => RuntimeSplice m (View v)
          -> Splice m
dfEncType getView = do
    return $ yieldRuntime $ do
        view <- getView
        return $ fromByteString $ encodeUtf8 $ T.pack (show $ viewEncType view)


dfMaster :: Monad m
         => (Text -> [(Text, Text)] -> View v -> RuntimeSplice m Builder)
         -> RuntimeSplice m (View v) -> Splice m
dfMaster f getView = do
    node <- getParamNode
    (ref, attrs) <- getRefAttributes node Nothing
    runAttrs <- runAttributesRaw attrs
    return $ yieldRuntime $ do
        view <- getView
        attrs' <- runAttrs
        f ref attrs' view


dfTag :: (Monad m)
      => (Text -> [(Text, Text)] -> Text -> [X.Node])
      -> RuntimeSplice m (View v)
      -> Splice m
dfTag f = dfMaster $ \ref attrs view -> do
    let ref' = absoluteRef ref view
        -- If there is no bang pattern on value, then for some reason it
        -- doesn't get forced and errors aren't displayed properly.
        !value = fieldInputText ref view
        attrs' = setDisabled ref view attrs
    return $ X.renderHtmlFragment X.UTF8 $ f ref' attrs' value


dfInputGeneric :: Monad m
               => [(Text, Text)]
               -> RuntimeSplice m (View v)
               -> Splice m
dfInputGeneric as = dfTag $ \ref attrs value ->
    makeElement "input" [] $ addAttrs attrs $
        as ++ [("id", ref), ("name", ref), ("value", value)]


--------------------------------------------------------------------------------
-- | Generate a submit button. Example:
--
-- > <dfInputSubmit />
dfInputSubmit :: Monad m => Splice m
dfInputSubmit = do
    node <- getParamNode
    (_, attrs) <- getRefAttributes node (Just "")
    runAttrs <- runAttributesRaw attrs
    return $ yieldRuntime $ do
        attrs' <- runAttrs
        let e = makeElement "input" [] $ addAttrs attrs'
                [("type", "submit")]
        return $ X.renderHtmlFragment X.UTF8 e


--------------------------------------------------------------------------------
-- | Generate a label for a field. Example:
--
-- > <dfLabel ref="user.married">Married: </dfLabel>
-- > <dfInputCheckbox ref="user.married" />
dfLabel :: Monad m => RuntimeSplice m (View v) -> Splice m
dfLabel getView = do
    node <- getParamNode
    (ref, attrs) <- getRefAttributes node Nothing
    runAttrs <- runAttributesRaw attrs
    return $ yieldRuntime $ do
        view <- getView
        attrs' <- runAttrs
        let ref'  = absoluteRef ref view
            e = makeElement "label" (X.childNodes node) $ addAttrs attrs'
                [("for", ref')]
        return $ X.renderHtmlFragment X.UTF8 e


--------------------------------------------------------------------------------
-- | Generate an input field with a supplied type. Example:
--
-- > <dfInput type="date" ref="date" />
dfInput :: Monad m => RuntimeSplice m (View v) -> Splice m
dfInput = dfInputGeneric []


--------------------------------------------------------------------------------
-- | Generate a text input field. Example:
--
-- > <dfInputText ref="user.name" />
dfInputText :: Monad m => RuntimeSplice m (View v) -> Splice m
dfInputText = dfInputGeneric [("type", "text")]


--------------------------------------------------------------------------------
-- | Generate a text area. Example:
--
-- > <dfInputTextArea ref="user.about" />
dfInputTextArea :: Monad m => RuntimeSplice m (View v) -> Splice m
dfInputTextArea = dfTag $ \ref attrs value ->
    makeElement "textarea" [X.TextNode value] $ addAttrs attrs
        [("id", ref), ("name", ref)]


--------------------------------------------------------------------------------
-- | Generate a password field. Example:
--
-- > <dfInputPassword ref="user.password" />
dfInputPassword :: Monad m => RuntimeSplice m (View v) -> Splice m
dfInputPassword = dfInputGeneric [("type", "password")]


--------------------------------------------------------------------------------
-- | Generate a hidden input field. Example:
--
-- > <dfInputHidden ref="user.forgery" />
dfInputHidden :: Monad m => RuntimeSplice m (View v) -> Splice m
dfInputHidden = dfInputGeneric [("type", "hidden")]


--------------------------------------------------------------------------------
-- | Generate a checkbox. Example:
--
-- > <dfInputCheckbox ref="user.married" />
dfInputCheckbox :: Monad m
                => RuntimeSplice m (View v)
                -> Splice m
dfInputCheckbox = dfMaster $ \ref attrs view -> do
    let ref'  = absoluteRef ref view
        value = fieldInputBool ref view
        e = makeElement "input" [] $ addAttrs attrs $
                   attr value ("checked", "checked") $
                   [("type", "checkbox"), ("id", ref'), ("name", ref')]

    return $ X.renderHtmlFragment X.UTF8 e


--------------------------------------------------------------------------------
-- | Generate a file upload element. Example:
--
-- > <dfInputFile ref="user.avatar" />
dfInputFile :: Monad m => RuntimeSplice m (View v) -> Splice m
dfInputFile = dfMaster $ \ref attrs view -> do
    let ref'  = absoluteRef ref view
        value = maybe "" T.pack $ fieldInputFile ref view
        e = makeElement "input" [] $ addAttrs attrs $
            [ ("type", "file"), ("id", ref')
            , ("name", ref'), ("value", value)]
    return $ X.renderHtmlFragment X.UTF8 e


--------------------------------------------------------------------------------
-- | Generate a select button (also known as a combo box). Example:
--
-- > <dfInputSelect ref="user.sex" />
dfInputSelect :: Monad m => RuntimeSplice m (View Text) -> Splice m
dfInputSelect = dfMaster $ \ref attrs view -> do
    let ref'     = absoluteRef ref view
        choices  = fieldInputChoice ref view
        kids     = map makeOption choices
        value i  = ref' <> "." <> i

        makeOption (i, c, sel) = X.Element "option"
            (attr sel ("selected", "selected") [("value", value i)])
            [X.TextNode c]

        e = makeElement "select" kids $ addAttrs attrs
            [("id", ref'), ("name", ref')]
    return $ X.renderHtmlFragment X.UTF8 e


--------------------------------------------------------------------------------
-- | Generate a select button (also known as a combo box). Example:
--
-- > <dfInputSelectGroup ref="user.sex" />
dfInputSelectGroup :: Monad m => RuntimeSplice m (View Text) -> Splice m
dfInputSelectGroup = dfMaster $ \ref attrs view -> do
    let ref'     = absoluteRef ref view
        choices  = fieldInputChoiceGroup ref view
        kids     = map makeGroup choices
        value i  = ref' <> "." <> i

        makeGroup (name, options) = X.Element "optgroup"
            [("label", name)] $ map makeOption options
        makeOption (i, c, sel) = X.Element "option"
            (attr sel ("selected", "selected") [("value", value i)])
            [X.TextNode c]

        e = makeElement "select" kids $ addAttrs attrs
            [("id", ref'), ("name", ref')]
    return $ X.renderHtmlFragment X.UTF8 e


--------------------------------------------------------------------------------
-- | Generate a number of radio buttons. Example:
--
-- > <dfInputRadio ref="user.sex" />
dfInputRadio :: Monad m => RuntimeSplice m (View Text) -> Splice m
dfInputRadio = dfMaster $ \ref attrs view -> do
    let ref'     = absoluteRef ref view
        choices  = fieldInputChoice ref view
        kids     = concatMap makeOption choices
        value i  = ref' <> "." <> i

        makeOption (i, c, sel) =
            [ X.Element "input"
                (attr sel ("checked", "checked") $ addAttrs attrs
                    [ ("type", "radio"), ("value", value i)
                    , ("id", value i), ("name", ref')
                    ]) []
            , X.Element "label" [("for", value i)] [X.TextNode c]
            ]

    return $ X.renderHtmlFragment X.UTF8 kids


--------------------------------------------------------------------------------
-- | Display the list of errors for a certain field. Example:
--
-- > <dfErrorList ref="user.name" />
-- > <dfInputText ref="user.name" />
dfErrorList :: MonadIO m => RuntimeSplice m (View Text) -> Splice m
dfErrorList getView = do
    node <- getParamNode
    tfp <- getTemplateFilePath
    return $ yieldRuntime $ do
        view <- getView
        (ref, attrs) <- getRefAttributes' tfp node Nothing
        let nodes = errorList (errors ref view) attrs
        return $ X.renderHtmlFragment X.UTF8 nodes


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
dfChildErrorList :: MonadIO m => RuntimeSplice m (View Text) -> Splice m
dfChildErrorList getView = do
    node <- getParamNode
    tfp <- getTemplateFilePath
    return $ yieldRuntime $ do
        view <- getView
        (ref, attrs) <- getRefAttributes' tfp node (Just "")
        let nodes = errorList (childErrors ref view) attrs
        return $ X.renderHtmlFragment X.UTF8 nodes


--------------------------------------------------------------------------------
-- | Render some content only if there are any errors. This is useful for markup
-- purposes.
--
-- > <dfIfChildErrors ref="user">
-- >     Content to be rendered if there are any errors...
-- > </dfIfChildErrors>
--
-- The @ref@ attribute can be omitted if you want to check the entire form.
dfIfChildErrors :: (MonadIO m) => RuntimeSplice m (View v) -> Splice m
dfIfChildErrors getView = do
    node <- getParamNode
    childrenChunks <- runChildren
    tfp <- getTemplateFilePath
    return $ yieldRuntime $ do
        view <- getView
        (ref, _) <- getRefAttributes' tfp node $ Just ""
        if null (childErrors ref view)
          then return mempty
          else codeGen childrenChunks


--------------------------------------------------------------------------------
-- | Render some content only if there are any errors. This is useful for markup
-- purposes.
--
-- > <dfIfNoChildErrors ref="user">
-- >     Content to be rendered if there are any errors...
-- > </dfIfNoChildErrors>
--
-- The @ref@ attribute can be omitted if you want to check the entire form.
dfIfNoChildErrors :: (MonadIO m) => RuntimeSplice m (View v) -> Splice m
dfIfNoChildErrors getView = do
    node <- getParamNode
    childrenChunks <- runChildren
    tfp <- getTemplateFilePath
    return $ yieldRuntime $ do
        view <- getView
        (ref, _) <- getRefAttributes' tfp node $ Just ""
        if null (childErrors ref view)
          then codeGen childrenChunks
          else return mempty


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
dfSubView :: MonadIO m => RuntimeSplice m (View Text) -> Splice m
dfSubView getView = do
    node <- getParamNode
    p2 <- newEmptyPromise
    tfp <- getTemplateFilePath
    let action = yieldRuntimeEffect $ do
            view <- getView
            (ref, _) <- getRefAttributes' tfp node Nothing
            let view' = subView ref view
            putPromise p2 view'
    res <- withLocalSplices (digestiveSplices (getPromise p2)) noSplices $
             runNodeList $ X.childNodes node
    return $ action <> res


------------------------------------------------------------------------------
-- | Splice that expands to a field's value.
dfValue :: Monad m => RuntimeSplice m (View v) -> Splice m
dfValue = dfMaster $ \ref _ view -> do
    let !value = fieldInputText ref view
    return $ fromByteString $ encodeUtf8 value


dfSingleListItem :: MonadIO n
                 => X.Node
                 -> (RuntimeSplice n (View Text) -> AttrSplice n)
                 -> RuntimeSplice n (View Text)
                 -> Splice n
dfSingleListItem node attrs viewPromise = do
    p2 <- newEmptyPromise
    let action = yieldRuntimeEffect $ do
            view <- viewPromise
            putPromise p2 view
    res <- withLocalSplices (digestiveSplices (getPromise p2))
                            ("itemAttrs" ## attrs viewPromise)
                            (runNodeList $ X.childNodes node)
    return $ action <> res


--------------------------------------------------------------------------------
-- | This splice allows variable length lists.  It binds several attribute
-- splices providing functionality for dynamically manipulating the list.  The
-- following descriptions will use the example of a form named \"foo\" with a
-- list subform named \"items\".
--
-- Splices:
--   dfListItem - This tag must surround the markup for a single list item.
--     It surrounds all of its children with a div with id \"foo.items\" and
--     class \"inputList\" and displays a copy for each of the list items
--     including a \"template\" item used for generating new items.  If the
--     you supply the attribute \"noTemplate\", then the template item is not
--     included and the generated list will not be dynamically updated by the
--     add and remove actions.
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
dfInputList :: MonadIO m => RuntimeSplice m (View Text) -> Splice m
dfInputList getView = do
    node <- getParamNode
    itemsPromise <- newEmptyPromise
    refPromise <- newEmptyPromise
    indicesPromise <- newEmptyPromise
    templateViewPromise <- newEmptyPromise
    tfp <- getTemplateFilePath
    let itemAttrs gv _ = do
            view <- gv
            listRef <- getPromise refPromise
            return
              [ ("data-ind", T.concat [listRef, ".", last $ "0" : viewContext view])
              , ("class", T.append listRef ".inputListItem")
              ]
        templateAttrs gv _ = do
            view <- gv
            listRef <- getPromise refPromise
            return
              [ ("data-ind", T.concat [listRef, ".", last $ "-1" : viewContext view])
              , ("class", T.append listRef ".inputListTemplate")
              , ("style", "display: none;")
              ]
        dfListItem = do
            n <- getParamNode
            template <- dfSingleListItem n templateAttrs (getPromise templateViewPromise)
            body <- deferMany (dfSingleListItem n itemAttrs) $
                              getPromise itemsPromise
            return $ if X.hasAttribute "noTemplate" n
              then body
              -- The inputListInstance class control dynamic update.  If we're
              -- not displaying a template, then we can't dynamically update.
              else mconcat [ yieldPureText "<div class=\"inputListInstance\">"
                           , template
                           , body
                           , yieldPureText "</div>"
                           ]
    let listAttrs =
            [ ("id", "${dfListRef}")
            , ("class", "inputList")
            ]
        indices = X.Element "input"
                    [ ("type", "hidden")
                    , ("name", T.intercalate "." ["${dfListRef}", indicesRef])
                    , ("value", "${dfIndicesList}")
                    ] []
        e = X.Element "div" listAttrs (indices : X.childNodes node)
    let addControl _ = do
            listRef <- getPromise refPromise
            return [ ("onclick", T.concat [ "addInputListItem(this, '"
                                          , listRef
                                          , "'); return false;"] ) ]
        removeControl _ = do
            listRef <- getPromise refPromise
            return [ ("onclick", T.concat [ "removeInputListItem(this, '"
                                          , listRef
                                          , "'); return false;"] ) ]
        attrSplices = do
            "addControl"    ## addControl
            "removeControl" ## removeControl
        splices = do
            "dfListRef"     ## return $ yieldRuntimeText $ getPromise refPromise
            "dfIndicesList" ## return $ yieldRuntimeText $ getPromise indicesPromise
            "dfListItem"    ## dfListItem

    -- The runtime action that gets the right data and puts it in promises.
    let action = yieldRuntimeEffect $ do
          view <- getView
          (ref, _) <- getRefAttributes' tfp node Nothing
          let listRef  = absoluteRef ref view
              items = listSubViews ref view
              tview = makeListSubView ref (-1) view
          putPromise refPromise listRef
          putPromise indicesPromise $ T.intercalate "," $
            map (last . ("0":) . viewContext) items
          putPromise itemsPromise items
          putPromise templateViewPromise tview

    -- At the core, this function just runs its children with a few splices
    -- and attribute splices bound.
    res <- withLocalSplices splices attrSplices $ runNode e
    return $ action <> res


--------------------------------------------------------------------------------
makeElement :: Text -> [X.Node] -> [(Text, Text)] -> [X.Node]
makeElement name nodes = return . flip (X.Element name) nodes


--------------------------------------------------------------------------------
-- | Does not override existing attributes
addAttrs :: [(Text, Text)]  -- ^ Original attributes
         -> [(Text, Text)]  -- ^ Attributes to add
         -> [(Text, Text)]  -- ^ Resulting attributes
addAttrs = unionBy (on (==) fst)


--------------------------------------------------------------------------------
errorList :: [Text] -> [(Text, Text)] -> [X.Node]
errorList []   _     = []
errorList errs attrs = [X.Element "ul" attrs $ map makeError errs]
  where
    makeError e = X.Element "li" [] [X.TextNode e]
