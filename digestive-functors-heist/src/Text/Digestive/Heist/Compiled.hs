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
import           Control.Monad         (mplus)
import           Control.Monad.Trans
import           Data.Function         (on)
import           Data.List             (unionBy)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>), mempty)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding
import           Heist
import           Heist.Compiled
import qualified Text.XmlHtml          as X


------------------------------------------------------------------------------
import           Text.Digestive.Form.List
import           Text.Digestive.View


------------------------------------------------------------------------------
-- | List of splices defined for forms.  For most uses the formSplice function
-- will be fine and you won't need to use this directly.  But this is
-- available if you need more customization.
digestiveSplices :: (Monad m) => Promise (View Text) -> [(Text, Splice m)]
digestiveSplices vp =
    [ ("dfInput",            dfInput vp)
    , ("dfInputText",        dfInputText vp)
    , ("dfInputTextArea",    dfInputTextArea vp)
    , ("dfInputPassword",    dfInputPassword vp)
    , ("dfInputHidden",      dfInputHidden vp)
    , ("dfInputSelect",      dfInputSelect vp)
    , ("dfInputSelectGroup", dfInputSelectGroup vp)
    , ("dfInputRadio",       dfInputRadio vp)
    , ("dfInputCheckbox",    dfInputCheckbox vp)
    , ("dfInputFile",        dfInputFile vp)
    , ("dfInputSubmit",      dfInputSubmit)
    , ("dfLabel",            dfLabel vp)
    , ("dfErrorList",        dfErrorList vp)
    , ("dfChildErrorList",   dfChildErrorList vp)
    , ("dfSubView",          dfSubView vp)
    , ("dfIfChildErrors",    dfIfChildErrors vp)
    , ("dfInputList",        dfInputList vp)
    , ("dfEncType",          dfEncType vp)
    ]


------------------------------------------------------------------------------
-- | A compiled splice for a specific form.  You pass in a runtime action that
-- gets the form's view and this function returns a splice that creates a form
-- tag.  In your HeistConfig you might have a compiled splice like this:
--
-- `("customerForm", formSplice (liftM fst $ runForm "customer" custForm))`
--
-- Then you can use the customerForm tag just like you would use the dfForm
-- tag in interpreted templates anywhere you want to have a customer form.
formSplice :: Monad m => m (View Text) -> Splice m
formSplice = formSplice' [] []


------------------------------------------------------------------------------
-- | A compiled splice for a specific form.  You pass in a runtime action that
-- gets the form's view and this function returns a splice that creates a form
-- tag.  In your HeistConfig you might have a compiled splice like this:
--
-- `("customerForm", formSplice (liftM fst $ runForm "customer" custForm))`
--
-- Then you can use the customerForm tag just like you would use the dfForm
-- tag in interpreted templates anywhere you want to have a customer form.
--formSplice' :: Monad m => m (View Text) -> Splice m
formSplice' :: Monad m
            => [(Text, Splice m)]
            -> [(Text, AttrSplice m)]
            -> m (View Text)
            -> Splice m
formSplice' ss as getView = do
    node <- getParamNode
    let (_, attrs) = getRefAttributes node Nothing
        tree = X.Element "form"
                 (addAttrs attrs
                    [ ("method", "POST")
                    , ("enctype", "${dfEncType}")
                    ])
                 (X.childNodes node)
        action = runNode tree
    defer (\vp -> withLocalSplices (digestiveSplices vp ++ ss) as action)
          (lift getView)


--------------------------------------------------------------------------------
attr :: Bool -> (Text, Text) -> [(Text, Text)] -> [(Text, Text)]
attr False _ = id
attr True  a = (a :)


--------------------------------------------------------------------------------
getRefAttributes :: X.Node
                 -> Maybe Text              -- ^ Optional default ref
                 -> (Text, [(Text, Text)])  -- ^ (Ref, other attrs)
getRefAttributes node defaultRef =
    case node of
        X.Element _ as _ ->
            let ref = fromMaybe (error $ show node ++ ": missing ref") $
                        lookup "ref" as `mplus` defaultRef
            in (ref, filter ((/= "ref") . fst) as)
        _                -> (error "Wrong type of node!", [])


dfEncType :: (Monad m)
          => Promise (View v)
          -> Splice m
dfEncType p = do
    return $ yieldRuntime $ do
        view <- getPromise p
        return $ fromByteString $ encodeUtf8 $ T.pack (show $ viewEncType view)


dfMaster :: Monad m
         => (Text -> [(Text, Text)] -> View v -> RuntimeSplice m Builder)
         -> Promise (View v) -> Splice m
dfMaster f p = do
    node <- getParamNode
    let (ref, attrs) = getRefAttributes node Nothing
    runAttrs <- runAttributesRaw attrs
    return $ yieldRuntime $ do
        view <- getPromise p
        attrs' <- runAttrs
        f ref attrs' view


dfTag :: (Monad m)
      => (Text -> [(Text, Text)] -> Text -> [X.Node])
      -> Promise (View v)
      -> Splice m
dfTag f = dfMaster $ \ref attrs view -> do
    let ref' = absoluteRef ref view
        -- If there is no bang pattern on value, then for some reason it
        -- doesn't get forced and errors aren't displayed properly.
        !value = fieldInputText ref view
    return $ X.renderHtmlFragment X.UTF8 $ f ref' attrs value


dfInputGeneric :: Monad m
               => [(Text, Text)]
               -> Promise (View v)
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
    let (_, attrs) = getRefAttributes node Nothing
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
dfLabel :: Monad m => Promise (View v) -> Splice m
dfLabel p = do
    node <- getParamNode
    let (ref, attrs) = getRefAttributes node Nothing
    runAttrs <- runAttributesRaw attrs
    return $ yieldRuntime $ do
        view <- getPromise p
        attrs' <- runAttrs
        let ref'  = absoluteRef ref view
            e = makeElement "label" (X.childNodes node) $ addAttrs attrs'
                [("for", ref')]
        return $ X.renderHtmlFragment X.UTF8 e


--------------------------------------------------------------------------------
-- | Generate an input field with a supplied type. Example:
--
-- > <dfInput type="date" ref="date" />
dfInput :: Monad m => Promise (View v) -> Splice m
dfInput = dfInputGeneric []


--------------------------------------------------------------------------------
-- | Generate a text input field. Example:
--
-- > <dfInputText ref="user.name" />
dfInputText :: Monad m => Promise (View v) -> Splice m
dfInputText = dfInputGeneric [("type", "text")]


--------------------------------------------------------------------------------
-- | Generate a text area. Example:
--
-- > <dfInputTextArea ref="user.about" />
dfInputTextArea :: Monad m => Promise (View v) -> Splice m
dfInputTextArea = dfTag $ \ref attrs value ->
    makeElement "textarea" [X.TextNode value] $ addAttrs attrs
        [("id", ref), ("name", ref)]


--------------------------------------------------------------------------------
-- | Generate a password field. Example:
--
-- > <dfInputPassword ref="user.password" />
dfInputPassword :: Monad m => Promise (View v) -> Splice m
dfInputPassword = dfInputGeneric [("type", "password")]


--------------------------------------------------------------------------------
-- | Generate a hidden input field. Example:
--
-- > <dfInputHidden ref="user.forgery" />
dfInputHidden :: Monad m => Promise (View v) -> Splice m
dfInputHidden = dfInputGeneric [("type", "hidden")]


--------------------------------------------------------------------------------
-- | Generate a checkbox. Example:
--
-- > <dfInputCheckbox ref="user.married" />
dfInputCheckbox :: Monad m
                => Promise (View v)
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
dfInputFile :: Monad m => Promise (View v) -> Splice m
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
dfInputSelect :: Monad m => Promise (View Text) -> Splice m
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
dfInputSelectGroup :: Monad m => Promise (View Text) -> Splice m
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
dfInputRadio :: Monad m => Promise (View Text) -> Splice m
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
dfErrorList :: Monad m => Promise (View Text) -> Splice m
dfErrorList p = do
    node <- getParamNode
    return $ yieldRuntime $ do
        view <- getPromise p
        let (ref, attrs) = getRefAttributes node Nothing
            nodes = errorList (errors ref view) attrs
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
dfChildErrorList :: Monad m => Promise (View Text) -> Splice m
dfChildErrorList p = do
    node <- getParamNode
    return $ yieldRuntime $ do
        view <- getPromise p
        let (ref, attrs) = getRefAttributes node (Just "")
            nodes = errorList (childErrors ref view) attrs
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
dfIfChildErrors :: (Monad m) => Promise (View v) -> Splice m
dfIfChildErrors p = do
    node <- getParamNode
    return $ yieldRuntime $ do
        view <- getPromise p
        let (ref, _) = getRefAttributes node Nothing
        if null (childErrors ref view)
          then return mempty
          else return $ X.renderHtmlFragment X.UTF8 (X.childNodes node)


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
dfSubView :: Monad m => Promise (View Text) -> Splice m
dfSubView p = do
    node <- getParamNode
    p2 <- newEmptyPromise
    let action = yieldRuntimeEffect $ do
            view <- getPromise p
            let (ref, _) = getRefAttributes node Nothing
                view' = subView ref view
            putPromise p2 view'
    res <- withLocalSplices (digestiveSplices p2) [] $
             runNodeList $ X.childNodes node
    return $ action <> res


dfSingleListItem :: Monad m
                 => X.Node
                 -> (Promise (View Text) -> AttrSplice m)
                 -> Promise (View Text)
                 -> HeistT m IO (RuntimeSplice m Builder)
dfSingleListItem node attrs viewPromise = do
    p2 <- newEmptyPromise
    let action = yieldRuntimeEffect $ do
            view <- getPromise viewPromise
            putPromise p2 view
    res <- withLocalSplices (digestiveSplices p2)
                            [("itemAttrs", attrs viewPromise)]
                            (runNodeList $ X.childNodes node)
    return $ codeGen $ action <> res


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
dfInputList :: Monad m => Promise (View Text) -> Splice m
dfInputList p = do
    node <- getParamNode
    itemsPromise <- newEmptyPromise
    refPromise <- newEmptyPromise
    indicesPromise <- newEmptyPromise
    templateViewPromise <- newEmptyPromise
    let itemAttrs viewPromise _ = do
            view <- getPromise viewPromise
            listRef <- getPromise refPromise
            return
              [ ("id", T.concat [listRef, ".", last $ "0" : viewContext view])
              , ("class", T.append listRef ".inputListItem")
              ]
        templateAttrs viewPromise _ = do
            view <- getPromise viewPromise
            listRef <- getPromise refPromise
            return
              [ ("id", T.concat [listRef, ".", last $ "-1" : viewContext view])
              , ("class", T.append listRef ".inputListTemplate")
              , ("style", "display: none;")
              ]
        dfListItem = do
            n <- getParamNode
            template <- dfSingleListItem n templateAttrs
                                         templateViewPromise
            body <- mapPromises (dfSingleListItem n itemAttrs) $
                                getPromise itemsPromise
            return $ yieldRuntime template <> body
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
        attrSplices = [ ("addControl", addControl)
                      , ("removeControl", removeControl)
                      ]
        splices = [ ("dfListRef", return $ yieldRuntimeText $ getPromise refPromise)
                  , ("dfIndicesList", return $ yieldRuntimeText $ getPromise indicesPromise)
                  , ("dfListItem", dfListItem)
                  ]

    -- The runtime action that gets the right data and puts it in promises.
    let action = yieldRuntimeEffect $ do
          view <- getPromise p
          let (ref, _) = getRefAttributes node Nothing
              listRef  = absoluteRef ref view
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


