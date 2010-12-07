Example illustrating happstack with digestive-functors. This requires the
digestive-functors-happstack package.

> {-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
> import Data.Maybe (fromMaybe)
> import Control.Applicative ((<$>), (<*>))
> import Control.Monad.Trans (liftIO)
> import Happstack.Server

> import Text.Blaze (Html, (!))
> import qualified Text.Blaze.Html5 as H
> import qualified Text.Blaze.Html5.Attributes as A
> import Text.Blaze.Renderer.Utf8 (renderHtml)

> import qualified Data.ByteString.Lazy as LB

> import Text.Digestive.Types
> import Text.Digestive.Blaze.Html5
> import Text.Digestive.Forms (formFileInputContents)
> import Text.Digestive.Forms.Happstack (eitherHappstackForm)

The next instance should not be needed once Happstack 0.6 is released, we'll
have to live with it for now.

> instance ToMessage Html where
>     toContentType _ = "text/html; charset=UTF-8"
>     toMessage = renderHtml

We're going to create a very simple file upload server.

> data Upload = Upload LB.ByteString String
>             deriving (Show)

> uploadForm :: (Monad m, Functor m) => Form m Input Html BlazeFormHtml Upload
> uploadForm = Upload
>     <$> fmap (fromMaybe "Unknown" . fmap formFileInputContents) inputFile
>     <*> label "Destination name: " ++> inputText Nothing

> upload :: ServerPart Response
> upload = do
>     r <- eitherHappstackForm (childErrors ++> uploadForm)  "upload-form"
>     case r of
>         Left form' -> ok $ toResponse $ do
>             let (formHtml', enctype) = renderFormHtml form'
>             H.form ! A.enctype (H.stringValue $ show enctype)
>                    ! A.method "POST" ! A.action "/" $ do
>                 formHtml'
>                 H.input ! A.type_ "submit" ! A.value "Upload"
>         Right (Upload lbs fileName) -> do
>             liftIO $ LB.writeFile ("/tmp/" ++ fileName) lbs
>             ok $ toResponse $ H.h1 "File uploaded"

> main :: IO ()
> main = simpleHTTP nullConf upload
