module Snap
    ( postFormSnap
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import qualified Data.Text.Encoding as T
import qualified Snap.Core as Snap

import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View

snapEnv :: Snap.MonadSnap m => Env m
snapEnv path =
    map T.decodeUtf8 . fromMaybe [] . M.lookup name <$> Snap.getParams
  where
    name = T.encodeUtf8 $ fromPath path

postFormSnap :: Snap.MonadSnap m => Form m v a -> m (Either (View m v) a)
postFormSnap form = postForm form snapEnv
