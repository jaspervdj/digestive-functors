-- | Module providing a happstack backend for the digestive-functors library
--
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Text.Digestive.Forms.Happstack
    ( HappstackForm
    , happstackEnvironment
    , eitherHappstackForm
    ) where

import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO(..))

import Data.ByteString.Lazy as LB
import Data.ByteString.Lazy.UTF8 as LB (toString)
import Happstack.Server (Input (..), getDataFn, lookInput, Method (..), rqMethod)
import Happstack.Server.Internal.Monads (ServerPartT, withRequest, runServerPartT)

import Text.Digestive.Forms (FormInput (..))
import Text.Digestive.Types (Form (..), Environment (..), viewForm, eitherForm)

instance FormInput Input (String, FilePath) where
    getInputString inp = case inputValue inp of
      (Right bs) -> Just . LB.toString $ bs
      _         -> Nothing
    getInputFile inp = case inputValue inp of
      (Left fp) -> inputFilename inp >>= \fn -> return (fn, fp)
      _         -> Nothing

-- | Simplification of the `Form` type, instantiated to Happstack
--
type HappstackForm m e v a = Form (ServerPartT m) Input e v a

-- | Environment that will fetch input from the parameters parsed by Happstack
--
{-
happstackEnvironment :: (Monad m) => Environment (ServerPartT m) Input
happstackEnvironment = Environment $ \id -> do
  res <- (getDataFn . lookInput . show) id
  case res of
    (Right a) -> return $ Just a
    _         -> return Nothing
-}
happstackEnvironment :: (Monad m, MonadIO m) => Environment (ServerPartT m) Input
happstackEnvironment = Environment $ \id -> do
  res <- getDataFn . lookInput . show $ id
  case res of
    (Right a) -> return (Just a)
    _         -> return Nothing

-- | Run a happstack form
--
-- * When we are responding to a GET request, you will simply receive the form
--   as a view
--
-- * When we are responding to another request method, the form data will be
--   used. When errors occur, you will receive the form as a view, otherwise,
--   you will get the actual result
--
eitherHappstackForm :: (Monad m, Functor m, MonadIO m)
                    => HappstackForm m e v a       -- ^ Form
                    -> String                      -- ^ Form name
                    -> ServerPartT m (Either v a)  -- ^ Result
eitherHappstackForm form name = withRequest $ \rq -> flip runServerPartT rq $
    case rqMethod rq of GET -> liftM Left $ viewForm form name
                        _   -> eitherForm form name happstackEnvironment
