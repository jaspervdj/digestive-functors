-- This code uses the convenience code I added to digestive-functors-blaze.
-- http://bit.ly/hjGRXl

{-# LANGUAGE OverloadedStrings, TypeSynonymInstances,
             NoMonomorphismRestriction #-}

import Control.Applicative
import Snap.Types
import Snap.Http.Server (httpServe, defaultConfig)
import Snap.Util.FileServe

import Text.Blaze (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderHtml)

import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms.Snap
import Text.Digestive.Types
import Text.Digestive.Transform

data Employee = Employee
    { name   :: String
    , age    :: Int
    , salary :: Double
    } deriving (Show)

eFormlet :: (Functor m, Monad m)
         => Formlet m SnapInput e BlazeFormHtml Employee
eFormlet d = Employee
    <$> label "Name" ++> inputText (fmap name d)
    <*> label "Age" ++> inputTextRead undefined (fmap age d)
    <*> label "Salary" ++> inputTextRead undefined (fmap salary d)

data Whole = Whole
    { firstField :: String
    , employees :: [Employee]
    , lastField :: String
    } deriving (Show)

whole :: Whole
whole = Whole "alpha"
              [Employee "John" 41 13.5
              ,Employee "Jane" 35 13.5]
              "omega"

formlet :: (Functor m, Monad m)
         => Formlet m SnapInput String BlazeFormHtml Whole
formlet d = Whole
    <$> label "First field" ++> inputText (fmap firstField d)
    <*> inputList hiddenInt eFormlet (fmap employees d)
    <*> label "Last field" ++> inputText (fmap lastField d)
  where
    hiddenInt = transformFormlet show inputHidden $ transformRead "Internal error"

blaze :: Html -> Snap ()
blaze response = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml $ htmlPage response

formHandler :: Snap ()
formHandler = do
    r <- eitherSnapForm (formlet (Just whole)) "employee-form"
    case r of
        Left form' -> blaze $ do
            let (formHtml', enctype) = renderFormHtml form'
            H.h1 "Add Employees"
            H.form ! A.enctype (H.toValue $ show enctype)
                   ! A.method "POST" ! A.action "/" $ do
                formHtml'
                H.div $ H.input ! A.type_ "submit" ! A.value "Submit"

        Right res -> blaze $ do
            H.h1 "Nothing yet"
            H.div $ H.toHtml $ show res

site :: Snap ()
site = ifTop formHandler
       <|> serveDirectory "."

htmlPage :: Html -> Html
htmlPage c = do
    H.html $ do
      H.head $ do
        jscript "/jquery-1.3.2.min.js"
        H.script ! A.type_ "text/javascript" $ H.toHtml inputListJs
      H.body c
  where
    jscript file =
        H.script ! A.type_ "text/javascript"
                 ! A.src file $ " "

main :: IO ()
main = httpServe defaultConfig site
