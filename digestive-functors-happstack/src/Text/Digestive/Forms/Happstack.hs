-- | Module providing a happstack backend for the digestive-functors library
--
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Digestive.Forms.Happstack
    ( HappstackForm
    , happstackEnvironment
    , eitherHappstackForm
    ) where

import Control.Monad (MonadPlus, liftM)
import Control.Applicative (Alternative, optional)

import Data.ByteString.Lazy.UTF8 as LB (toString)
import Data.Text.Lazy          as Text (toStrict)
import Data.Text.Lazy.Encoding as Text (decodeUtf8)
import Happstack.Server ( Input (..), HasRqData (..), lookInput
                        , Method (..), ServerMonad (..), rqMethod
                        )

import Text.Digestive.Forms (FormInput (..))
import Text.Digestive.Types (Form (..), Environment (..), viewForm, eitherForm)

instance FormInput Input (String, FilePath) where
    getInputStrings inp =
        case inputValue inp of
          (Right bs) -> return . LB.toString $ bs
          _          -> []
    getInputTexts inp =
        case inputValue inp of
          (Right bs) -> return . Text.toStrict . Text.decodeUtf8 $ bs
          _          -> []

    getInputFile inp =
        case inputValue inp of
          (Left fp) ->
              do fn <- inputFilename inp
                 return (fn, fp)
          _ -> Nothing

-- | Simplification of the `Form` type, instantiated to Happstack
--
type HappstackForm m e v a = Form m Input e v a

-- | Environment that will fetch input from the parameters parsed by Happstack
--
happstackEnvironment :: (HasRqData m, MonadPlus m, Alternative m)
                     => Environment m Input
happstackEnvironment = Environment $ optional . lookInput . show

-- | Run a happstack form
--
-- * When we are responding to a GET request, you will simply receive the form
--   as a view
--
-- * When we are responding to another request method, the form data will be
--   used. When errors occur, you will receive the form as a view, otherwise,
--   you will get the actual result
--
eitherHappstackForm :: (HasRqData m, MonadPlus m, Alternative m, ServerMonad m)
                    => HappstackForm m e v a       -- ^ Form
                    -> String                      -- ^ Form name
                    -> m (Either v a)              -- ^ Result
eitherHappstackForm form name = askRq >>= \rq ->
    case rqMethod rq of GET -> liftM Left $ viewForm form name
                        _   -> eitherForm form name happstackEnvironment
