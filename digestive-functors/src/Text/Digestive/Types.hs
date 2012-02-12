-- | Core types
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Digestive.Types
    ( View (..)
    , Environment (..)
    , fromList
    , FormState
    , getFormId
    , getFormRange
    , getFormInput
    , getFormInput'
    , isFormInput
    , Form (..)
    , Formlet
    , bracketState
    , incState
    , view
    , (++>)
    , (<++)
    , mapView
    , runForm
    , runViewForm
    , eitherForm
    , viewForm
    ) where

import Data.Monoid (Monoid (..))
import Control.Arrow (first)
import Control.Monad (liftM, liftM2, mplus)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Trans (lift)
import Control.Applicative (Applicative (..))

import Text.Digestive.Result

-- | A view represents a visual representation of a form. It is composed of a
-- function which takes a list of all errors and then produces a new view
--
newtype View e v = View
    { unView :: [(FormRange, e)] -> v
    } deriving (Monoid)

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

-- | Create an environment from a lookup table
--
fromList :: Monad m => [(FormId, i)] -> Environment m i
fromList list = Environment $ return . flip lookup list

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
getFormInput = getFormId >>= getFormInput'

-- | Gets the input of an arbitrary FormId.
--
getFormInput' :: Monad m => FormId -> FormState m i (Maybe i)
getFormInput' id' = do
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
newtype Form m i e v a = Form
    { unForm :: FormState m i (View e v, m (Result e a))
    }

-- | A function for generating forms with an optional default value.
--
type Formlet m i e v a = Maybe a -> Form m i e v a

instance Monad m => Functor (Form m i e v) where
    fmap f form = Form $ do
        (view', result) <- unForm form
        return (view', liftM (fmap f) result)

instance (Monad m, Monoid v) => Applicative (Form m i e v) where
    pure x = Form $ return (mempty, return (return x))
    f1 <*> f2 = Form $ do
        -- Assuming f1 already has a valid ID
        ((v1,r1), (v2,r2)) <- bracketState $ do
            res1 <- unForm f1
            incState
            res2 <- unForm f2
            return (res1, res2)
        return (v1 `mappend` v2, liftM2 (<*>) r1 r2)

bracketState :: Monad m => FormState m i a -> FormState m i a
bracketState k = do
    FormRange startF1 _ <- get
    res <- k
    FormRange _ endF2 <- get
    put $ FormRange startF1 endF2
    return res

incState :: Monad m => FormState m i ()
incState = do
        FormRange _ endF1 <- get
        put $ unitRange endF1

-- | Insert a view into the functor
--
view :: Monad m
     => v                -- ^ View to insert
     -> Form m i e v ()  -- ^ Resulting form
view view' = Form $ return (View (const view'), return (Ok ()))

-- | Append a unit form to the left. This is useful for adding labels or error
-- fields
--
(++>) :: (Monad m, Monoid v)
      => Form m i e v ()
      -> Form m i e v a
      -> Form m i e v a
f1 ++> f2 = Form $ do
    -- Evaluate the form that matters first, so we have a correct range set
    (v2, r) <- unForm f2
    (v1, _) <- unForm f1
    return (v1 `mappend` v2, r)

infixl 6 ++>

-- | Append a unit form to the right. See '++>'.
--
(<++) :: (Monad m, Monoid v)
      => Form m i e v a
      -> Form m i e v ()
      -> Form m i e v a
f1 <++ f2 = Form $ do
    -- Evaluate the form that matters first, so we have a correct range set
    (v1, r) <- unForm f1
    (v2, _) <- unForm f2
    return (v1 `mappend` v2, r)

infixr 5 <++

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
        => Form m i e v a                -- ^ Form to run
        -> String                        -- ^ Identifier for the form
        -> Environment m i               -- ^ Input environment
        -> m (View e v, m (Result e a))  -- ^ Result
runForm form id' env = evalStateT (runReaderT (unForm form) env) $
    unitRange $ zeroId id'

-- | Evaluate a form to it's view if it fails
--
eitherForm :: Monad m
           => Form m i e v a   -- ^ Form to run
           -> String           -- ^ Identifier for the form
           -> Environment m i  -- ^ Input environment
           -> m (Either v a)   -- ^ Result
eitherForm form id' env = do
    (view', mresult) <- runForm form id' env
    result <- mresult
    return $ case result of
        Error e  -> Left $ unView view' e
        Ok x     -> Right x

-- | Evaluate a form, return view and a result if successful
runViewForm :: Monad m
            => Form m i e v a
            -> String
            -> Environment m i
            -> m (v, Maybe a)
runViewForm form id' env = do
    (view', mresult) <- runForm form id' env
    result <- mresult
    return $ case result of
        Error e -> (unView view' e, Nothing)
        Ok x    -> (unView view' [], Just x)

-- | Just evaluate the form to a view. This usually maps to a GET request in the
-- browser.
--
viewForm :: Monad m
         => Form m i e v a  -- ^ Form to run
         -> String          -- ^ Identifier for the form
         -> m v             -- ^ Result
viewForm form id' = do
    (view', _) <- runForm form id' NoEnvironment
    return $ unView view' []
