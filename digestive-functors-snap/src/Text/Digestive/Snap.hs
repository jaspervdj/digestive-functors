-- | Module providing a Snap backend for the digestive-functors library
module Text.Digestive.Snap
    ( runForm
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Snap.Core as Snap

import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View

snapEnv :: Snap.MonadSnap m => Env m
snapEnv path =
    map (TextInput . T.decodeUtf8) . findParams <$> Snap.getPostParams
  where
    findParams = fromMaybe [] . M.lookup name 
    name       = T.encodeUtf8 $ fromPath path

-- | Runs a form with the HTTP input provided by Happstack.
--
-- Automatically picks between 'getForm' and 'postForm' based on the request
-- method.
runForm :: Snap.MonadSnap m
        => Text                 -- ^ Name for the form
        -> Form v m a           -- ^ Form to run
        -> m (View v, Maybe a)  -- ^ Result
runForm name form = Snap.getRequest >>= \rq ->
    case Snap.rqMethod rq of
        Snap.GET -> return (getForm name form, Nothing)
        _        -> postForm name form snapEnv
