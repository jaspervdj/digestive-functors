module Snap
    ( postFormSnap
    ) where

import qualified Data.Text.Encoding as T
import qualified Snap.Core as Snap

import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View

snapEnv :: Snap.MonadSnap m => Env m
snapEnv = fmap (fmap T.decodeUtf8) . Snap.getParam . T.encodeUtf8 . fromPath

postFormSnap :: Snap.MonadSnap m => Form m v a -> m (Either (View m v) a)
postFormSnap form = postForm form snapEnv
