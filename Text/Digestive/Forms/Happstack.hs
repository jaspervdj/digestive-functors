-- | Module providing a happstack backend for the digestive-functors library
--
module Text.Digestive.Forms.Happstack
    ( happstackEnvironment
    , eitherHappstackForm
    ) where

import Control.Arrow ((&&&))
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)

import Data.ByteString.Lazy.UTF8 as LB (toString)
import Happstack.Server ( Input (..), ServerPartT, getDataFn, lookInput
                        , Method (..), withRequest, runServerPartT, rqMethod
                        )

import Text.Digestive.Forms (FormInput (..))
import Text.Digestive.Types (Form (..), Environment (..), viewForm, eitherForm)

instance FormInput Input where
    getInputString = LB.toString . inputValue
    getInputFile = fromMaybe "" . inputFilename &&& inputValue

-- | Environment that will fetch input from the parameters parsed by Happstack
--
happstackEnvironment :: Monad m => Environment (ServerPartT m) Input
happstackEnvironment = Environment $ getDataFn . lookInput . show

-- | Run a happstack form
--
-- * When we are responding to a GET request, you will simply receive the form
--   as a view
--
-- * When we are responding to another request method, the form data will be
--   used. When errors occur, you will receive the form as a view, otherwise,
--   you will get the actual result
--
eitherHappstackForm :: (Monad m, Functor m)
                    => Form (ServerPartT m) Input e v a  -- ^ Form
                    -> String                            -- ^ Form name
                    -> ServerPartT m (Either v a)        -- ^ Result
eitherHappstackForm form name = withRequest $ \rq -> flip runServerPartT rq $
    case rqMethod rq of GET -> liftM Left $ viewForm form name
                        _   -> eitherForm form name happstackEnvironment
