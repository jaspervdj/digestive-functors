-- | Module providing a happstack backend for the digestive-functors library
module Text.Digestive.Happstack
    ( runForm
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

-- | Runs a form with the HTTP input provided by Happstack.
--
-- Automatically picks between 'getForm' and 'postForm' based on the request
-- method.
runForm :: (Happstack.HasRqData m, Monad m, Happstack.ServerMonad m)
        => Form v m a -> m (View v, Maybe a)
runForm form = Happstack.askRq >>= \rq ->
    case Happstack.rqMethod rq of
        Happstack.GET -> return $ (getForm form, Nothing)
        _             -> postForm form happstackEnv
