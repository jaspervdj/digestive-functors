{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (liftIO)
import Happstack.Server
import Data.Monoid (mappend)
import System.Directory (copyFile)

import Text.Blaze (Html, toHtml)
import qualified Text.Blaze.Html5 as H

import Text.Digestive.Blaze.Html5
import Text.Digestive.Form
import Text.Digestive.Forms.Happstack
import Text.Digestive.View

data Upload = Upload (Maybe FilePath) String
    deriving (Show)

uploadForm :: Form (ServerPartT IO) Html Upload
uploadForm = Upload
    <$> "file" .: file
    <*> "name" .: string Nothing

uploadView :: View m Html -> Html
uploadView view = form view "/" $ do
    label "file" "File: "
    inputFile "file" view
    H.br

    label "name" "Destination filename: "
    inputText "name" view
    H.br

    inputSubmit "Upload"

upload :: ServerPart Response
upload = do
    decodeBody $ defaultBodyPolicy "/tmp/" 32000 1000 1000
    r <- eitherForm uploadForm
    case r of
        Left view -> ok $ toResponse $ uploadView view
        Right (Upload (Just fileName) destination) -> do
            liftIO $ copyFile fileName destination
            ok $ toResponse $ do
                H.h1 "File uploaded"
                H.p $ "Location: " `mappend` toHtml destination
        Right (Upload Nothing _) ->
            ok $ toResponse $ H.h1 "No file given"

main :: IO ()
main = simpleHTTP nullConf upload
