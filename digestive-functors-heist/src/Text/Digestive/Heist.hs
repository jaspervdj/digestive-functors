{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Heist
    ( digestiveSplices
    , bindDigestiveSplices
    ) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)

import Data.Text (Text)
import Text.Digestive.View
import Text.Templating.Heist
import qualified Data.Text as T
import qualified Text.XmlHtml as X

attr :: Bool -> (Text, Text) -> [(Text, Text)] -> [(Text, Text)]
attr False _ = id
attr True  a = (a :)

makeElement :: Text -> [X.Node] -> [(Text, Text)] -> [X.Node]
makeElement name nodes = return . flip (X.Element name) nodes

getRefAttributes :: Monad m => HeistT m (Text, [(Text, Text)])
getRefAttributes = do
    node <- getParamNode
    return $ case node of
        X.Element _ as _ ->
            let ref = fromMaybe (error $ show node ++ ": missing ref") $
                        lookup "ref" as
            in (ref, filter ((/= "ref") . fst) as)
        _                -> (error "Wrong type of node!", [])

inputText :: Monad m => View v -> Splice m
inputText view = do
    (ref, attrs) <- getRefAttributes
    let ref'  = absoluteRef ref view
        value = fieldInputText ref view
    return $ makeElement "input" [] $
        ("type", "text") : ("id", ref') :
        ("name", ref') : ("value", value) : attrs

inputTextArea :: Monad m => View v -> Splice m
inputTextArea view = do
    (ref, attrs) <- getRefAttributes
    let ref'  = absoluteRef ref view
        value = fieldInputText ref view
    return $ makeElement "textarea" [X.TextNode value] $
        ("id", ref') : ("name", ref') : attrs

inputPassword :: Monad m => View v -> Splice m
inputPassword view = do
    (ref, attrs) <- getRefAttributes
    let ref'  = absoluteRef ref view
        value = fieldInputText ref view
    return $ makeElement "input" [] $
        ("type", "password") : ("id", ref') :
        ("name", ref') : ("value", value) : attrs

inputHidden :: Monad m => View v -> Splice m
inputHidden view = do
    (ref, attrs) <- getRefAttributes
    let ref'  = absoluteRef ref view
        value = fieldInputText ref view
    return $ makeElement "input" [] $
        ("type", "hidden") : ("id", ref') :
        ("name", ref') : ("value", value) : attrs

inputSelect :: Monad m => View Text -> Splice m
inputSelect view = do
    (ref, attrs) <- getRefAttributes
    let ref'           = absoluteRef ref view
        (choices, idx) = fieldInputChoice ref view
        children       = zipWith makeOption choices [0 ..]
        value i        = ref' `mappend` "." `mappend` T.pack (show i)
        makeOption c i = X.Element "option"
            (attr (idx == i) ("selected", "selected") [("value", value i)])
            [X.TextNode c]
    return $ makeElement "select" children $
        ("id", ref') : ("name", ref') : attrs

inputRadio :: Monad m => View Text -> Splice m
inputRadio view = do
    (ref, attrs) <- getRefAttributes

    let ref'           = absoluteRef ref view
        (choices, idx) = fieldInputChoice ref view

        children       = concat $ zipWith makeOption choices [0 ..]
        value i        = ref' `mappend` "." `mappend` T.pack (show i)
        makeOption c i =
            [ X.Element "input"
                (attr (idx == i) ("checked", "checked") $
                    ("type", "radio") : ("value", value i) :
                    ("id",    value i) : ("name", ref') :
                    attrs ) []
            , X.Element "label" [("for", value i)] [X.TextNode c]
            ]

    return children

inputCheckbox :: Monad m => View Text -> Splice m
inputCheckbox view = do
    (ref, attrs) <- getRefAttributes
    let ref'  = absoluteRef ref view
        value = fieldInputBool ref view
    return $ makeElement "input" [] $ attr value ("checked", "checked") $
        ("type", "checkbox") : ("id", ref') : ("name", ref') : attrs

inputSubmit :: Monad m => View v -> Splice m
inputSubmit _ = do
    (_, attrs) <- getRefAttributes
    return $ makeElement "input" [] $ ("type", "submit") : attrs

label :: Monad m => View v -> Splice m
label view = do
    (ref, attrs) <- getRefAttributes
    content      <- liftM X.childNodes getParamNode
    let ref' = absoluteRef ref view
    return $ makeElement "label" content $ ("for", ref') : attrs

form :: Monad m => View v -> Splice m
form view = do
    (_, attrs) <- getRefAttributes
    content    <- liftM X.childNodes getParamNode
    return $ makeElement "form" content $
        ("method", "POST") :
        ("enctype", T.pack (show $ viewEncType view)) :
        attrs

errorList' :: [Text] -> [(Text, Text)] -> [X.Node]
errorList' []   _     = []
errorList' errs attrs = [X.Element "ul" attrs $ map makeError errs]
  where
    makeError e = X.Element "li" [] [X.TextNode e]

errorList :: Monad m => View Text -> Splice m
errorList view = do
    (ref, attrs) <- getRefAttributes
    return $ errorList' (errors ref view) attrs

childErrorList :: Monad m => View Text -> Splice m
childErrorList view = do
    (ref, attrs) <- getRefAttributes
    return $ errorList' (childErrors ref view) attrs

bindDigestiveSplices :: Monad m => View Text -> HeistState m -> HeistState m
bindDigestiveSplices = bindSplices . digestiveSplices

digestiveSplices :: Monad m => View Text -> [(Text, Splice m)]
digestiveSplices view =
    [ ("dfInputText",      inputText view)
    , ("dfInputTextArea",  inputTextArea view)
    , ("dfInputPassword",  inputPassword view)
    , ("dfInputHidden",    inputHidden view)
    , ("dfInputSelect",    inputSelect view)
    , ("dfInputRadio",     inputRadio view)
    , ("dfInputCheckbox",  inputCheckbox view)
    , ("dfInputSubmit",    inputSubmit view)
    , ("dfLabel",          label view)
    , ("dfForm",           form view)
    , ("dfErrorList",      errorList view)
    , ("dfChildErrorList", childErrorList view)
    ]
