-- | Module providing a snap backend for the digestive-functors library
--
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Text.Digestive.Forms.Snap
    ( SnapInput
    , SnapForm
    , snapEnvironment
    , eitherSnapForm
    ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)

import Data.ByteString as SB
import Data.ByteString.UTF8 as SB (toString, fromString)
import Snap.Types

import Text.Digestive.Forms (FormInput (..))
import Text.Digestive.Types (Form (..), Environment (..), viewForm, eitherForm)

newtype SnapInput = SnapInput {unSnapInput :: SB.ByteString}

instance FormInput SnapInput () where
    getInputString = Just . SB.toString . unSnapInput
    getInputFile = const Nothing

-- | Simplification of the `Form` type, instantiated to Snap
--
type SnapForm m = Form m SnapInput

-- | Environment that will fetch input from the parameters parsed by Snap
--
snapEnvironment :: (MonadSnap m) => Environment m SnapInput
snapEnvironment = Environment $ \id' -> do
    input' <- getParam (SB.fromString $ show id')
    return $ SnapInput <$> input'

-- | Run a snap form
--
-- * When we are responding to a GET request, you will simply receive the form
--   as a view
--
-- * When we are responding to another request method, the form data will be
--   used. When errors occur, you will receive the form as a view, otherwise,
--   you will get the actual result
--
eitherSnapForm :: (MonadSnap m)
               => SnapForm m e v a     -- ^ Form
               -> String             -- ^ Form name
               -> m (Either v a)  -- ^ Result
eitherSnapForm form name = do
    method' <- rqMethod <$> getRequest
    case method' of GET -> liftM Left $ viewForm form name
                    _   -> eitherForm form name snapEnvironment
