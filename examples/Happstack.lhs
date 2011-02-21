Example illustrating happstack with digestive-functors. This requires the
digestive-functors-happstack package.

> {-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
> import Data.Maybe (fromMaybe)
> import Control.Applicative ((<$>), (<*>))
> import Control.Monad.Trans (liftIO)
> import Happstack.Server
> import Data.Monoid (mappend)
> import System.Directory (copyFile)

> import Text.Blaze (Html, (!), toHtml)
> import qualified Text.Blaze.Html5 as H
> import qualified Text.Blaze.Html5.Attributes as A
> import Text.Blaze.Renderer.Utf8 (renderHtml)

> import qualified Data.ByteString.Lazy as LB

> import Text.Digestive.Types
> import Text.Digestive.Blaze.Html5
> import Text.Digestive.Forms.Happstack

We're going to create a very simple file upload server.

> data Upload = Upload FilePath String
>             deriving (Show)

> uploadForm :: (Monad m, Functor m)
>            => HappstackForm m Html BlazeFormHtml Upload
> uploadForm = Upload
>     <$> fmap (fromMaybe "Unknown" . fmap snd) inputFile
>     <*> label "Destination name: " ++> inputText Nothing

> upload :: ServerPart Response
> upload = do
>     decodeBody $ defaultBodyPolicy "/tmp/" 32000 1000 1000
>     r <- eitherHappstackForm (childErrors ++> uploadForm)  "upload-form"
>     case r of
>         Left form' -> ok $ toResponse $ do
>             let (formHtml', enctype) = renderFormHtml form'
>             H.form ! A.enctype (H.stringValue $ show enctype)
>                    ! A.method "POST" ! A.action "/" $ do
>                 formHtml'
>                 H.input ! A.type_ "submit" ! A.value "Upload"
>         Right (Upload fileName destination) -> do
>             liftIO $ copyFile fileName destination
>             ok $ toResponse $ do
>                 H.h1 "File uploaded"
>                 H.p $ "Location: " `mappend` toHtml destination

> main :: IO ()
> main = simpleHTTP nullConf upload
