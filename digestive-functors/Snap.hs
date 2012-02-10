{-# LANGUAGE OverloadedStrings #-}
module Snap where

import Control.Applicative (Applicative (..), (<$>))
import Data.Text (Text)
import Text.Blaze (Html)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Snap.Blaze as Snap
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Text.Blaze.Html5 as H

import Field
import Html

postFormSnap :: Snap.MonadSnap m => Form m v a -> m (Either (View m v a) a)
postFormSnap form = postForm form $
    fmap (fmap T.decodeUtf8) . Snap.getParam . T.encodeUtf8 . fromPath

data User = User Text Int Sex
    deriving (Show)

data Sex = Female | Male
    deriving (Eq, Show)

userForm :: Monad m => Form m Html User
userForm = User
    <$> "name" .: text (Just "jasper")
    <*> "age"  .: check "Should be positive" (> 0) (stringRead (Just 21))
    <*> "sex"  .: choice [(Female, "Female"), (Male, "Male")] (Just Male)

userView :: View m Html a -> Html
userView v = form "/test" $ do
    label "name" "Name: "
    inputText "name" v
    errorList "name" v

    label "age" "Age: "
    inputText "age" v
    errorList "age" v

    -- label "sex" "Sex: "
    inputRadio False "sex" v
    errorList "sex" v

    inputSubmit "Submit"

getTest :: Snap.Snap ()
getTest = Snap.blaze $ userView $ getForm
    (userForm :: Form Snap.Snap H.Html User)

postTest :: Snap.Snap ()
postTest = do
    r <- postFormSnap userForm
    case r of
        Left view -> Snap.blaze $ userView view
        Right x   -> Snap.blaze $ H.toHtml $ show x

site :: Snap.Snap ()
site = Snap.route
    [ ("/test", Snap.method Snap.GET  getTest)
    , ("/test", Snap.method Snap.POST postTest)
    ]

main :: IO ()
main = Snap.httpServe Snap.defaultConfig site
