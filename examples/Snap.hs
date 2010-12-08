{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans (liftIO)
import Control.Applicative ((<|>), (<*>), (<$>), (*>), (<*))
import System.Environment (getArgs)
import Data.Monoid (mempty, mappend)
import Data.Char (isLower)

import Codec.Binary.UTF8.String (decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import Snap.Types
import Snap.Http.Server (httpServe)
import Snap.Util.FileServe (fileServe)

import Text.Digestive.Types
import Text.Digestive.Validate
import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms
import Text.Digestive.Result
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Blaze (Html, string, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

type SnapForm a = Form Snap String Html BlazeFormHtml a

newtype SnapInput a = SnapInput (Snap a)

instance FormInput SnapInput where
    getInputString :: 

-- | Request a user
--
runSnapForm :: Show a => SnapForm a -> Snap ()
runSnapForm form = do
    method <- rqMethod <$> getRequest
    params <- rqParams <$> getRequest
    case method of
        GET -> do
            view' <- viewForm form "some-form"
            blaze $ blazeRoot $ blazeForm "/" $ fst $ renderFormHtml view'
        _ -> do
            x <- eitherForm form "some-form" env
            case x of
                Left html -> blaze $ blazeRoot $ blazeForm "/" $ fst $ renderFormHtml html
                Right user -> blaze $ blazeRoot $ H.div ! A.class_ "succes" $ string $ show user 
  where
    env = Environment $ \id' -> do
        val <- getParam (SBC.pack (show id'))
        return $ fmap (decode . SB.unpack) val

blazeRoot :: Html -> Html
blazeRoot inner = H.docTypeHtml $ do
    H.head $ do
        H.title "digestive demo"
        H.link ! A.href "/screen.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.body $ do
        H.a ! A.href "/" $ H.h1 "Digestive demo"
        inner

blazeForm :: ByteString -> Html -> Html
blazeForm url inner = H.form ! A.method "post"
                             ! A.action (H.unsafeByteStringValue url)
                             $ inner

blaze :: Html -> Snap ()
blaze html = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml html

-- | Site handler
--
site :: Snap ()
site = fileServe "static" <|> route [ ("/", runSnapForm bookingFormDemo)
                                    ] 

-- | Main function
--
main :: IO ()
main = do
    args <- getArgs
    let port = case args of [p] -> read p
                            _   -> 8000
    httpServe "*" port "tweetov" Nothing (Just "error.log") site

-------------------------------------------------------------------------------
-- DEMO                                                                      --
-------------------------------------------------------------------------------

data Booking = Booking Int Int
             deriving (Show)

bookingForm :: (Monad m, Functor m)
            => Form m String Html BlazeFormHtml Booking
bookingForm = Booking
    <$> label "Arrival: " ++> inputTextRead "No read" Nothing <++ errors
    <*> label "Departure: " ++> inputTextRead "No read" Nothing <++ errors

bookingFormDemo :: (Monad m, Functor m)
                => Form m String Html BlazeFormHtml Booking
bookingFormDemo = errors ++> bookingForm `validate` correctBooking

correctBooking :: Monad m => Validator m Html Booking
correctBooking = check "Time machine detected!" $ \(Booking x y) -> x < y
