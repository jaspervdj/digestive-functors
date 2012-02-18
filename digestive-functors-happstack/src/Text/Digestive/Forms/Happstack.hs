-- | Module providing a happstack backend for the digestive-functors library
--
module Text.Digestive.Forms.Happstack
    ( postFormHappstack
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
        Left filePath -> FileUpload filePath
        Right bs      -> TextInput $ TL.toStrict $ TL.decodeUtf8 bs

postFormHappstack :: (Happstack.HasRqData m, Monad m)
                  => Form m v a -> m (Either (View m v) a)
postFormHappstack form = postForm form happstackEnv
