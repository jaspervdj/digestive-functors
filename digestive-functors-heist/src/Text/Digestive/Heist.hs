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
    return [X.Element "label" (("for", ref') : attrs) content]

inputText :: Monad m => View v -> Splice m
inputText view = do
    (ref, attrs) <- getRefAttributes
    let ref'  = absoluteRef ref view
        value = fieldInputText ref view
    return [X.Element "input"
        (("id", ref') : ("name", ref') : ("value", value) : attrs) []]

inputSubmit :: Monad m => View v -> Splice m
inputSubmit _ = do
    (_, attrs) <- getRefAttributes
    return [X.Element "input" (("type", "submit") : attrs) []]

bindDigestiveSplices :: Monad m => View v -> HeistState m -> HeistState m
bindDigestiveSplices view = bindSplices
    [ ("dfLabel",       label view)
    , ("dfInputText",   inputText view)
    , ("dfInputSubmit", inputSubmit view)
    ]
