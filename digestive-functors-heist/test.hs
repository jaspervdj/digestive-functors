{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import Control.Applicative ((<$>), (<*>))

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.Lens.Template
import Data.Text (Text)
import Snap.Core
import Snap.Http.Server (defaultConfig, httpServe)
import Snap.Snaplet
import Snap.Snaplet.Heist
import System.IO (hPutStrLn, stderr)
import Text.Digestive
import Text.Digestive.Heist
import Text.Digestive.Snap
import Text.Templating.Heist
import qualified Data.Text as T

data App = App
    { _heist :: Snaplet (Heist App)
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App

data User = User
    { userName :: Text
    , userAge  :: Int
    } deriving (Show)

userForm :: Monad m => Form Text m User
userForm = User
    <$> "name" .: text (Just "Jasper")
    <*> "age"  .: stringRead "Can't parse age" Nothing

form :: Handler App App ()
form = do
    (view, result) <- runForm "form" userForm
    case result of
        Just x  -> heistLocal (bindUser x) $ render "user"
        Nothing -> heistLocal (bindDigestiveSplices view) $ render "form"
  where
    bindUser user = bindStrings
        [ ("name", userName user)
        , ("age", T.pack (show $ userAge user))
        ]

routes :: [(ByteString, Handler App App ())]
routes = [("/", form)]

app :: SnapletInit App App
app = makeSnaplet "app" "digestive-functors testing application" Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    addRoutes routes
    return $ App h

main :: IO ()
main = do
    (msgs, site, cleanup) <- runSnaplet app
    hPutStrLn stderr $ T.unpack msgs
    _ <- try $ httpServe defaultConfig site :: IO (Either SomeException ())
    cleanup
