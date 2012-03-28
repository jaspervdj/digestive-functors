{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Heist
    ( bindDigestiveSplices
    ) where

import Control.Monad (liftM)

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Digestive.View
import Text.Templating.Heist
import qualified Text.XmlHtml as X

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

label :: Monad m => View v -> Splice m
label view = do
    (ref, attrs) <- getRefAttributes
    content      <- liftM X.childNodes getParamNode
    let ref' = absoluteRef ref view
    return $ makeElement "label" content $ ("for", ref') : attrs

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

inputSubmit :: Monad m => View v -> Splice m
inputSubmit _ = do
    (_, attrs) <- getRefAttributes
    return $ makeElement "input" [] $ ("type", "submit") : attrs

bindDigestiveSplices :: Monad m => View v -> HeistState m -> HeistState m
bindDigestiveSplices view = bindSplices
    [ ("dfLabel",         label view)
    , ("dfInputText",     inputText view)
    , ("dfInputTextArea", inputTextArea view)
    , ("dfInputPassword", inputPassword view)
    , ("dfInputHidden",   inputHidden view)
    , ("dfInputSubmit",   inputSubmit view)
    ]
