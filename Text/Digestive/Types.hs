-- | Cote types
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Digestive.Types where

import Data.Monoid (Monoid (..))
import Control.Arrow (first)
import Control.Monad (liftM2, mplus)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Trans (lift)
import Control.Applicative (Applicative (..))

-- | Type for failing computations
--
data Result e ok = Error [(FormRange, e)]
                 | Ok ok
                 deriving (Show)

instance Functor (Result e) where
    fmap _ (Error x) = Error x
    fmap f (Ok x) = Ok (f x)

instance Monad (Result e) where
    return = Ok
    Error x >>= _ = Error x
    Ok x >>= f = f x

instance Applicative (Result e) where
    pure = Ok
    Error x <*> Error y = Error $ x ++ y
    Error x <*> Ok _ = Error x
    Ok _ <*> Error y = Error y
    Ok x <*> Ok y = Ok $ x y

instance Monoid (Result e a) where
    mempty = Error []
    Error x `mappend` Error y = Error $ x ++ y
    Error _ `mappend` Ok x = Ok x
    Ok x `mappend` _ = Ok x

-- | An ID used to identify forms
--
newtype FormId = FormId {unFormId :: Integer}
               deriving (Eq, Ord, Num)

instance Show FormId where
    show (FormId x) = 'f' : show x

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
retainErrors :: FormRange -> [(FormRange, e)] -> [e]
retainErrors range = map snd . filter ((== range) . fst)

-- | Select the errors originating from this form or from any of the children of
-- this form
--
retainChildErrors :: FormRange -> [(FormRange, e)] -> [e]
retainChildErrors range = map snd . filter ((`isSubRange` range) . fst)

-- | A view represents a visual representation of a form. It is composed of a
-- function which takes a list of all errors and then produces a new view
--
newtype View e v = View
    { unView :: [(FormRange, e)] -> v
    }

instance Monoid v => Monoid (View e v) where
    mempty = View $ const mempty
    mappend (View f) (View g) = View $ \err -> mappend (f err) (g err)

instance Functor (View e) where
    fmap f (View g) = View $ f . g

-- | The environment is where you get the actual input per form. The environment
-- itself is optional
--
data Environment m i = Environment (FormId -> m (Maybe i))
                     | NoEnvironment

instance Monad m => Monoid (Environment m i) where
    mempty = NoEnvironment
    NoEnvironment `mappend` x = x
    x `mappend` NoEnvironment = x
    (Environment env1) `mappend` (Environment env2) = Environment $ \id' ->
        liftM2 mplus (env1 id') (env2 id')

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
    case env of Environment f -> lift $ lift $ f id'
                NoEnvironment -> return Nothing

-- | Check if any form input is present
--
isFormInput :: Monad m => FormState m i Bool
isFormInput = ask >>= \env -> return $ case env of
    Environment _ -> True
    NoEnvironment -> False

-- | A form represents a number of composed fields
--
newtype Form m i e v a = Form {unForm :: FormState m i (View e v, Result e a)}

instance Monad m => Functor (Form m i e v) where
    fmap f form = Form $ do
        (view', result) <- unForm form
        return (view', fmap f result)

instance (Monad m, Monoid v) => Applicative (Form m i e v) where
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

instance (Monad m, Monoid v) => Monoid (Form m i e v a) where
    mempty = Form $ return (mempty, mempty)
    f1 `mappend` f2 = Form $ do
        (v1, r1) <- unForm f1
        (v2, r2) <- unForm f2
        return (v1 `mappend` v2, r1 `mappend` r2)

-- | Insert a view into the functor
--
view :: Monad m
     => v               -- ^ View to insert
     -> Form m i e v a  -- ^ Resulting form
view view' = Form $ return (View (const view'), mempty)

-- | Change the view of a form using a simple function
--
mapView :: (Monad m, Functor m)
        => (v -> w)        -- ^ Manipulator
        -> Form m i e v a  -- ^ Initial form
        -> Form m i e w a  -- ^ Resulting form
mapView f = Form . fmap (first $ fmap f) . unForm

-- | Run a form
--
runForm :: Monad m
        => Form m i e v a
        -> Environment m i
        -> m (View e v, Result e a)
runForm form env = evalStateT (runReaderT (unForm form) env) $ FormRange 0 1

-- | Evaluate a form to it's view if it fails
--
eitherForm :: Monad m
           => Form m i e v a
           -> Environment m i
           -> m (Either v a)
eitherForm form env = do
    (view', result) <- runForm form env
    return $ case result of Error e -> Left $ unView view' e
                            Ok x    -> Right x
