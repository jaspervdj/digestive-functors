-- | Module providing a Snap backend for the digestive-functors library
module Text.Digestive.Snap
    ( runForm
    , runFormWith 
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe,catMaybes)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Snap.Core as Snap
import qualified Snap.Util.FileUploads as Snap

import Text.Digestive.Form
import Text.Digestive.Form.Encoding
import Text.Digestive.Types
import Text.Digestive.View


import System.Directory (getTemporaryDirectory,copyFile)
import System.FilePath (takeFileName,(</>))

type SnapPartPolicy = (Snap.PartInfo -> Snap.PartUploadPolicy)

snapEnv :: Snap.MonadSnap m => [(Text, FilePath)] -> Env m
snapEnv allfiles path = do
    inputs <- map (TextInput . T.decodeUtf8) . findParams <$> Snap.getPostParams
    let files = map (FileInput . snd) (filter ((== name) . fst) allfiles)
    return (inputs ++ files)
  where
    findParams = fromMaybe [] . M.lookup (T.encodeUtf8 name) 
    name       = fromPath path

-- | Runs a form with the HTTP input provided by Snap.
--
-- Automatically picks between 'getForm' and 'postForm' based on the request
-- method.
runForm :: Snap.MonadSnap m
        => Text                 -- ^ Name for the form
        -> Form v m a           -- ^ Form to run
        -> m (View v, Maybe a)  -- ^ Result
runForm = runFormWith Snap.defaultUploadPolicy 
                      (const $ Snap.allowWithMaximumSize (128 * 1024)) 
  
-- | Runs a form with a custom upload policy, and HTTP input from snap.
--
-- Automatically picks between 'getForm' and 'postForm' based on request 
-- method.
runFormWith :: Snap.MonadSnap m
            => Snap.UploadPolicy    -- ^ Upload policy
            -> SnapPartPolicy       -- ^ Per-part upload policy
            -> Text                 -- ^ Name for the form
            -> Form v m a           -- ^ Form to run
            -> m (View v, Maybe a)  -- ^ Result
runFormWith policy partpolicy name form = 
    Snap.getRequest >>= \rq ->
      case Snap.rqMethod rq of
        Snap.GET -> return (getForm name form, Nothing)
        _        -> case formEncType form of
          UrlEncoded -> postForm name form (snapEnv [])
          MultiPart -> do
            tmpdir <- liftIO getTemporaryDirectory
            files <- Snap.handleFileUploads tmpdir policy partpolicy (storeFiles tmpdir)
            postForm name form (snapEnv files) 
  where 
    storeFiles tmp = (fmap catMaybes . mapM (storeFile tmp))
    storeFile _   (_,       Left _)     = return Nothing
    storeFile tmp (partinfo,Right path) = do
      let newpath = tmp </> ("_" ++ (takeFileName path) ++ (maybe "" B.unpack $ Snap.partFileName partinfo))
      liftIO $ copyFile path newpath
      return $ Just (T.decodeUtf8 $ Snap.partFieldName partinfo, newpath)