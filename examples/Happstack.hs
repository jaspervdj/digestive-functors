{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
import Data.Maybe (fromMaybe)
import Control.Monad (liftM, msum, mplus)
import Control.Arrow ((&&&))
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (liftIO)
import Happstack.Server
import Text.Blaze as B
import Text.Blaze.Html4.Strict as B hiding (map, label)
import Text.Blaze.Html4.Strict.Attributes as B hiding (dir, title, label)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Control.Monad.Reader.Class (ask)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Text.Digestive.Types
import Text.Digestive.Forms (FormInput (..))
import Text.Digestive.Blaze.Html5

instance ToMessage B.Html where
    toContentType _ = "text/html; charset=UTF-8"
    toMessage = renderHtml

instance FormInput Input where
    getInputString = UTF8.toString . inputValue
    getInputFile = fromMaybe "" . inputFilename &&& inputValue

happstackEnvironment :: Monad m => Environment (ServerPartT m) Input
happstackEnvironment = Environment $ getDataFn . lookInput . show

data Upload = Upload UTF8.ByteString String
            deriving (Show)

uploadForm :: (Monad m, Functor m) => Form m Input Html BlazeFormHtml Upload
uploadForm = Upload <$> fmap (fromMaybe "Unknown" . fmap snd) inputFile
                    <*> label "Destination name: " ++> inputText Nothing

eitherHappstackForm :: (Monad m, Functor m)
                    => Form (ServerPartT m) Input Html BlazeFormHtml a
                    -> String
                    -> ServerPartT m (Either BlazeFormHtml a)
eitherHappstackForm form name = withRequest $ \rq -> flip runServerPartT rq $
    case rqMethod rq of GET -> liftM Left $ viewForm form name
                        _   -> eitherForm form name happstackEnvironment

upload :: ServerPart Response
upload = do
    result <- eitherHappstackForm (childErrors ++> uploadForm)  "upload-form"
    case result of
        Left form' -> ok $ toResponse $ do
            let (formHtml, enctype) = renderFormHtml form'
            form ! B.enctype (stringValue $ show enctype) ! B.method "POST" !  action "/" $ do
                formHtml
                input ! type_ "submit" ! value "Upload"
        Right (Upload lbs fileName) -> do
            liftIO $ LBS.writeFile fileName lbs            
            ok $ toResponse $ h1 "File uploaded"

main :: IO ()
main = simpleHTTP nullConf upload
