-- | Module providing a happstack backend for the digestive-functors library
--
module Text.Digestive.Forms.Happstack
    ( eitherForm
    ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Happstack.Server as Happstack

import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View

happstackEnv :: (Happstack.HasRqData m, Monad m) => Env m
happstackEnv path = do
    inputs <- Happstack.lookInputs $ T.unpack $ fromPath path
    return $ map toFormInput inputs
  where
    toFormInput input = case Happstack.inputValue input of
        Left filePath -> FileInput filePath
        Right bs      -> TextInput $ TL.toStrict $ TL.decodeUtf8 bs

eitherForm :: (Happstack.HasRqData m, Monad m, Happstack.ServerMonad m)
           => Form m v a -> m (Either (View m v) a)
eitherForm form = Happstack.askRq >>= \rq ->
    case Happstack.rqMethod rq of
        Happstack.GET -> return $ Left $ getForm form
        _             -> postForm form happstackEnv
