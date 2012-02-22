-- | This module mostly meant for internal usage, and might change between minor
-- releases.
{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings, Rank2Types #-}
module Text.Digestive.Form.Internal
    ( Form (..)
    , SomeForm (..)
    , transform
    , children
    , (.:)
    , lookupForm
    , toField
    , queryField
    , eval
    , formMapView
    ) where

import Control.Applicative (Applicative (..))
import Control.Monad ((>=>))
import Data.Maybe (listToMaybe, maybeToList)
import Data.Monoid (Monoid)

import Data.Text (Text)

import Text.Digestive.Types
import Text.Digestive.Field

type Ref = Maybe Text

-- | Base type for a form.
--
-- The three type parameters are:
--
-- * @v@: the type for textual information, displayed to the user. For example,
--   error messages are of this type. @v@ stands for "view".
--
-- * @m@: the monad in which validators operate. The classical example is when
--   validating input requires access to a database, in which case this @m@
--   should be an instance of @MonadIO@.
--
-- * @a@: the type of the value returned by the form, used for its Applicative
--   instance.
--
data Form v m a where
    Pure :: Ref -> Field v a -> Form v m a
    App  :: Ref -> Form v m (b -> a) -> Form v m b -> Form v m a

    Map  :: (b -> m (Result v a)) -> Form v m b -> Form v m a

instance Monad m => Functor (Form v m) where
    fmap = transform . (return .) . (return .)

instance (Monad m, Monoid v) => Applicative (Form v m) where
    pure x  = Pure Nothing (Singleton x)
    x <*> y = App Nothing x y

instance Show (Form v m a) where
    show = unlines . showForm

data SomeForm v m = forall a. SomeForm (Form v m a)

instance Show (SomeForm v m) where
    show (SomeForm f) = show f

showForm :: Form v m a -> [String]
showForm form = case form of
    (Pure r x)  -> ["Pure (" ++ show r ++ ") (" ++ show x ++ ")"]
    (App r x y) -> concat
        [ ["App (" ++ show r ++ ")"]
        , map indent (showForm x)
        , map indent (showForm y)
        ]
    (Map _ x)   -> "Map _" : map indent (showForm x)
  where
    indent = ("  " ++)

transform :: Monad m => (a -> m (Result v b)) -> Form v m a -> Form v m b
transform f (Map g x) = flip Map x $ \y -> bindResult (g y) f
transform f x         = Map f x

children :: Form v m a -> [SomeForm v m]
children (Pure _ _)  = []
children (App _ x y) = [SomeForm x, SomeForm y]
children (Map _ x)   = children x

setRef :: Ref -> Form v m a -> Form v m a
setRef r (Pure _ x)  = Pure r x
setRef r (App _ x y) = App r x y
setRef r (Map f x)   = Map f (setRef r x)

-- | Operator to set a name for a subform.
(.:) :: Text -> Form v m a -> Form v m a
(.:) = setRef . Just
infixr 5 .:

getRef :: Form v m a -> Ref
getRef (Pure r _)  = r
getRef (App r _ _) = r
getRef (Map _ x)   = getRef x

lookupForm :: Path -> Form v m a -> [SomeForm v m]
lookupForm path = go path . SomeForm
  where
    go []       form            = [form]
    go (r : rs) (SomeForm form) = case getRef form of
        Just r'
            -- Note how we use `setRef Nothing` to strip the ref away. This is
            -- really important.
            | r == r' && null rs -> [SomeForm $ setRef Nothing form]
            | r == r'            -> children form >>= go rs
            | otherwise          -> []
        Nothing                  -> children form >>= go (r : rs)

toField :: Form v m a -> Maybe (SomeField v)
toField (Pure _ x) = Just (SomeField x)
toField (Map _ x)  = toField x
toField _          = Nothing

queryField :: Path
           -> Form v m a
           -> (forall b. Field v b -> Maybe c)
           -> Maybe c
queryField path form f = do
    SomeForm form'  <- listToMaybe $ lookupForm path form
    SomeField field <- toField form'
    f field

ann :: Path -> Result v a -> Result [(Path, v)] a
ann _    (Success x) = Success x
ann path (Error x)   = Error [(path, x)]

eval :: Monad m => Method -> Env m -> Form v m a
     -> m (Result [(Path, v)] a, [(Path, FormInput)])
eval = eval' []

eval' :: Monad m => Path -> Method -> Env m -> Form v m a
      -> m (Result [(Path, v)] a, [(Path, FormInput)])

eval' context method env form = case form of

    Pure Nothing _ ->
        error "No ref specified for field"

    Pure (Just _) field -> do
        val <- env path
        let x = evalField method val field
        return $ (pure x, [(path, v) | v <- val])

    App _ x y -> do
        (x', inp1) <- eval' path method env x
        (y', inp2) <- eval' path method env y
        return (x' <*> y', inp1 ++ inp2)

    Map f x -> do
        (x', inp) <- eval' context method env x
        x''       <- bindResult (return x') (f >=> return . ann path)
        return (x'', inp)

  where
    path = context ++ maybeToList (getRef form)

formMapView :: Monad m => (v -> w) -> Form v m a -> Form w m a
formMapView f (Pure r x)  = Pure r (fieldMapView f x)
formMapView f (App r x y) = App r (formMapView f x) (formMapView f y)
formMapView f (Map g x)   = Map (g >=> return . resultMapError f) (formMapView f x)

-- | Utility: bind for 'Result' inside another monad
bindResult :: Monad m
           => m (Result v a) -> (a -> m (Result v b)) -> m (Result v b)
bindResult mx f = do
    x <- mx
    case x of
        Error errs  -> return $ Error errs
        Success x'  -> f x'
