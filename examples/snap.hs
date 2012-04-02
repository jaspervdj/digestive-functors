{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (liftIO)
import Data.Monoid (mappend)
import System.Directory (copyFile)

import Snap.Blaze (blaze)
import Snap.Core (Snap)
import Snap.Http.Server (defaultConfig, httpServe)
import Text.Blaze (Html, toHtml)
import qualified Text.Blaze.Html5 as H

import Text.Digestive.Blaze.Html5
import Text.Digestive.Form
import Text.Digestive.Snap
import Text.Digestive.View

data Upload = Upload (Maybe FilePath) String
    deriving (Show)

uploadForm :: Monad m => Form Html m Upload
uploadForm = Upload
    <$> "file" .: file
    <*> "name" .: check "Name can't be empty" (not . null) (string Nothing)

uploadView :: View Html -> Html
uploadView view = form view "/" $ do
    childErrorList "" view

    label     "file" view "File: "
    inputFile "file" view
    H.br

    label     "name" view "Destination filename: "
    inputText "name" view
    H.br

    inputSubmit "Upload"

upload :: Snap ()
upload = do
    r <- runForm "upload" uploadForm
    case r of
        (_, Just (Upload (Just fileName) destination)) -> do
            liftIO $ copyFile fileName destination
            blaze $ do
                H.h1 "File uploaded"
                H.p $ "Location: " `mappend` toHtml destination
        (view, _) -> blaze $ uploadView view

main :: IO ()
main = httpServe defaultConfig upload
