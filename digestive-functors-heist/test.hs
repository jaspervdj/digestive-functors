{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import Control.Applicative ((<$>), (<*>))

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.Lens.Template
import Data.Text (Text)
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

data Employment = Employed | Unemployed | Student
    deriving (Bounded, Enum, Eq, Show)

employments :: [(Employment, Text)]
employments = [(e, T.pack (show e)) | e <- [minBound .. maxBound]]

data Date = Date
    { dateDay   :: Int
    , dateMonth :: Int
    , dateYear  :: Int
    } deriving (Show)

dateForm :: Monad m => Form Text m Date
dateForm = check "Not a valid date" validDate $ Date
    <$> "day"   .: stringRead "Not a number" (Just 16)
    <*> "month" .: stringRead "Not a number" (Just 6)
    <*> "year"  .: stringRead "Not a number" (Just 1990)
  where
    validDate (Date day month _) =
        day   >= 1 && day <= 31 &&
        month >= 1 && month <= 12

data User = User
    { userName      :: Text
    , userPassword  :: Text
    , userBirthdate :: Date
    } deriving (Show)

userForm :: Monad m => Form Text m User
userForm = User
    <$> "name"      .: text (Just "Jasper")
    <*> "password"  .: text Nothing
    <*> "birthdate" .: dateForm

form :: Handler App App ()
form = do
    (view, result) <- runForm "form" userForm
    case result of
        Just x  -> heistLocal (bindUser x) $ render "user"
        Nothing -> heistLocal (bindDigestiveSplices view) $ render "user-form"
  where
    bindUser = bindString "user" . T.pack . show

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
