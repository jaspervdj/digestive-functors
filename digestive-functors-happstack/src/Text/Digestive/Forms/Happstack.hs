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
import Data.Either
import Data.Text.Lazy          as Text (toStrict)
import Data.Text.Lazy.Encoding as Text (decodeUtf8)
import Happstack.Server ( Input (..), HasRqData (..), lookInputs
                        , Method (..), ServerMonad (..), rqMethod
                        )

import Text.Digestive.Forms (FormInput (..))
import Text.Digestive.Types (Form (..), Environment (..), viewForm, eitherForm)

instance FormInput [Input] (String, FilePath) where
    getInputStrings inps =
        map LB.toString $ rights $ map inputValue inps

    getInputTexts inps =
        map (Text.toStrict . Text.decodeUtf8) $ rights $ map inputValue inps

    getInputFile inps =
        case inps of
          (inp:_) ->
              case inputValue inp of
                (Left fp) ->
                    do fn <- inputFilename inp
                       return (fn, fp)
                _ -> Nothing
          _ -> Nothing

-- | Simplification of the `Form` type, instantiated to Happstack
--
type HappstackForm m = Form m [Input]

-- | Environment that will fetch input from the parameters parsed by Happstack
--
happstackEnvironment :: (HasRqData m, MonadPlus m, Alternative m)
                     => Environment m [Input]
happstackEnvironment = Environment $ fmap toMaybe . lookInputs . show
    where
      toMaybe :: [a] -> Maybe [a]
      toMaybe [] = Nothing
      toMaybe l  = Just l

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
