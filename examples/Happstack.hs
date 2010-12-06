{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
import Data.Maybe                         (fromMaybe)
import Control.Monad                      (liftM, msum, mplus)
import Control.Applicative                ((<$>), (<*>))
import Happstack.Server
import Text.Blaze                         as B
import Text.Blaze.Html4.Strict            as B hiding (map)
import Text.Blaze.Html4.Strict.Attributes as B hiding (dir, title) 
import Text.Blaze.Renderer.Utf8           (renderHtml)

import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Text.Digestive.Types
import Text.Digestive.Http
import Text.Digestive.Blaze.Html5

instance ToMessage B.Html where
    toContentType _ = "text/html; charset=UTF-8"
    toMessage = renderHtml

instance HttpInput Input where
    getInputString = UTF8.toString . inputValue

happstackEnvironment :: Monad m => Environment (ServerPartT m) Input
happstackEnvironment = Environment $ getDataFn . lookInput . show

data User = User String Int
          deriving (Show)

userForm :: (Monad m, Functor m) => Form m Input Html BlazeFormHtml User
userForm = User <$> inputText (Just "username")
                <*> inputTextRead "Can't read" Nothing

eitherHappstackForm :: (Monad m, Functor m)
                    => Form (ServerPartT m) Input Html BlazeFormHtml a
                    -> String
                    -> ServerPartT m (Either BlazeFormHtml a)
eitherHappstackForm form name = dir "get" get' `mplus` post'
  where
    get' = liftM Left $ viewForm form name
    post' = eitherForm form name happstackEnvironment

user :: ServerPart Response
user = do
    result <- eitherHappstackForm (userForm <++ childErrors) "user-form"
    ok $ toResponse $ case result of
        Left form' -> form ! B.method "POST" ! action "/" $ do
            fst $ renderFormHtml form'
            input ! type_ "submit" ! value "Submit"
        Right user -> p $ string $ show user

main :: IO ()
main = simpleHTTP nullConf $ user
