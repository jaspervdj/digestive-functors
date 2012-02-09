{-# LANGUAGE OverloadedStrings #-}
module Snap where

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
