-- | Cote types
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Digestive.Types where

import Data.Monoid (Monoid (..))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans (lift)
import Control.Applicative (Applicative (..))

-- | Type for failing computations
--
data Result ok = Error [(FormRange, String)]
               | Ok ok
               deriving (Show)

instance Functor Result where
    fmap _ (Error x) = Error x
    fmap f (Ok x) = Ok (f x)

instance Monad Result where
    return = Ok
    Error x >>= _ = Error x
    Ok x >>= f = f x

instance Applicative Result where
    pure = Ok
    Error x <*> Error y = Error $ x ++ y
    Error x <*> Ok _ = Error x
    Ok _ <*> Error y = Error y
    Ok x <*> Ok y = Ok $ x y

-- | An ID used to identify forms
--
newtype FormId = FormId {unFormId :: Integer}
               deriving (Eq, Ord, Show, Num)

-- | A range of ID's to specify a group of forms
--
data FormRange = FormRange FormId FormId
             deriving (Show)

-- | A view represents a visual representation of a form. It is composed of a
-- function which takes a list of all errors and then produces a new view
--
newtype View v = View
    { unView :: [(FormRange, String)] -> v
    }

instance Monoid v => Monoid (View v) where
    mempty = View $ const mempty
    mappend (View f) (View g) = View $ \err -> mappend (f err) (g err)

-- | The environment is where you get the actual input per form
--
newtype Environment m i = Environment
    { unEnvironment :: FormId -> m (Maybe i)
    }

-- | The form state is a state monad under which our applicatives are composed
--
type FormState m i a = ReaderT (Environment m i) (StateT FormRange m) a

-- | A form represents a number of composed fields
--
newtype Form m i v a = Form {unForm :: FormState m i (View v, Result a)}

instance Monad m => Functor (Form m inp v) where
    fmap f form = Form $ do
        (view, result) <- unForm form
        return (view, fmap f result)

instance (Monad m, Monoid v) => Applicative (Form m inp v) where
    pure x = Form $ return (mempty, return x)
    f1 <*> f2 = Form $ do
        -- Assuming f1 already has a valid ID
        FormRange startF1 _ <- get
        (v1, r1) <- unForm f1
        FormRange _ endF1 <- get

        -- Set a new, empty range
        put $ FormRange endF1 $ endF1 + 1
        (v2, r2) <- unForm f2
        FormRange _ endF2 <- get

        put $ FormRange startF1 endF2
        return (v1 `mappend` v2, r1 <*> r2)

-- | Utility function: returns the current 'FormId'. This will only make sense
-- if the form is not composed
--
getFormId :: Monad m => FormState m i FormId
getFormId = do
    FormRange x _ <- get
    return x

-- | Utility function: Get the current range
--
getFormRange :: Monad m => FormState m i FormRange
getFormRange = get

-- | Utility function: Get the current input
--
getFormInput :: Monad m => FormState m i (Maybe i)
getFormInput = do
    id' <- getFormId
    env <- ask
    lift $ lift $ unEnvironment env id'
