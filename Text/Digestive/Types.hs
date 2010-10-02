-- | Cote types
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Digestive.Types where

import Data.Monoid (Monoid (..))
import Control.Monad (liftM2, mplus)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, evalStateT)
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
               deriving (Eq, Show)

-- | Check if a 'FormId' is contained in a 'FormRange'
--
isInRange :: FormId     -- ^ Id to check for
          -> FormRange  -- ^ Range
          -> Bool       -- ^ If the range contains the id
isInRange a (FormRange b c) = a >= b && a < c

-- | Check if a 'FormRange' is contained in another 'FormRange'
--
isSubRange :: FormRange  -- ^ Sub-range
           -> FormRange  -- ^ Larger range
           -> Bool       -- ^ If the sub-range is contained in the larger range
isSubRange (FormRange a b) (FormRange c d) = a >= c && b <= d

-- | Select the errors for a certain range
--
retainExactErrors :: FormRange -> [(FormRange, String)] -> [String]
retainExactErrors range = map snd . filter ((== range) . fst)

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

instance Monad m => Monoid (Environment m i) where
    mempty = Environment $ const $ return Nothing
    (Environment f1) `mappend` (Environment f2) = Environment $ \id' ->
        liftM2 mplus (f1 id') (f2 id')

-- | The form state is a state monad under which our applicatives are composed
--
type FormState m i a = ReaderT (Environment m i) (StateT FormRange m) a

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

-- | Change the view of a form using a function which takes
--
-- * The current form range
--
-- * The list of all errors (TODO: filter?)
--
-- * The original view
--
mapView :: Monad m
        => (FormRange -> [(FormRange, String)] -> v -> v)  -- ^ Manipulator
        -> Form m i v a                                    -- ^ Initial form
        -> Form m i v a                                    -- ^ Resulting form
mapView f form = Form $ do
    (view, result) <- unForm form
    range <- getFormRange
    return (View (\err -> f range err (unView view err)), result)

-- | Run a form
--
runForm :: Monad m => Form m i v a -> Environment m i -> m (View v, Result a)
runForm form env = evalStateT (runReaderT (unForm form) env) $ FormRange 0 1

-- | Evaluate a form to it's view if it fails
--
eitherForm :: Monad m => Form m i v a -> Environment m i -> m (Either v a)
eitherForm form env = do
    (view, result) <- runForm form env
    return $ case result of Error e -> Left $ unView view e
                            Ok x    -> Right x
