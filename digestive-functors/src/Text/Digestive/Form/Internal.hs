-- | This module mostly meant for internal usage, and might change between minor
-- releases.
{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings, Rank2Types #-}
module Text.Digestive.Form.Internal
    ( Form (..)
    , SomeForm (..)
    , transform
    , (.:)
    , lookupForm
    , toField
    , queryField
    , eval
    ) where

import Control.Applicative (Applicative (..))
import Control.Monad ((>=>))
import Data.Maybe (listToMaybe, maybeToList)
import Data.Monoid (Monoid)

import Data.Text (Text)

import Text.Digestive.Types
import Text.Digestive.Field
import Text.Digestive.Util

type Ref = Maybe Text

-- | Base type for a form.
--
-- The three type parameters are:
--
-- * @m@: the monad in which validators operate. The classical example is when
--   validating input requires access to a database, in which case this @m@
--   should be an instance of @MonadIO@.
--
-- * @v@: the type for textual information, displayed to the user. For example,
--   error messages are of this type. @v@ stands for "view".
--
-- * @a@: the type of the value returned by the form, used for its Applicative
--   instance.
--
data Form m v a where
    Pure :: Ref -> Field v a -> Form m v a
    App  :: Ref -> Form m v (b -> a) -> Form m v b -> Form m v a

    Map  :: (b -> m (Result v a)) -> Form m v b -> Form m v a

instance Monad m => Functor (Form m v) where
    fmap = transform . (return .) . (return .)

instance (Monad m, Monoid v) => Applicative (Form m v) where
    pure x  = Pure Nothing (Singleton x)
    x <*> y = App Nothing x y

instance Show (Form m v a) where
    show = unlines . showForm

data SomeForm m v = forall a. SomeForm (Form m v a)

instance Show (SomeForm m v) where
    show (SomeForm f) = show f

showForm :: Form m v a -> [String]
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

transform :: Monad m => (a -> m (Result v b)) -> Form m v a -> Form m v b
transform f (Map g x) = flip Map x $ \y -> bindResult (g y) f
transform f x         = Map f x

children :: Form m v a -> [SomeForm m v]
children (Pure _ _)  = []
children (App _ x y) = [SomeForm x, SomeForm y]
children (Map _ x)   = children x

setRef :: Ref -> Form m v a -> Form m v a
setRef r (Pure _ x)  = Pure r x
setRef r (App _ x y) = App r x y
setRef r (Map f x)   = Map f (setRef r x)

(.:) :: Text -> Form m v a -> Form m v a
(.:) = setRef . Just
infixr 5 .:

getRef :: Form m v a -> Ref
getRef (Pure r _)  = r
getRef (App r _ _) = r
getRef (Map _ x)   = getRef x

lookupForm :: Path -> Form m v a -> [SomeForm m v]
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

toField :: Form m v a -> Maybe (SomeField v)
toField (Pure _ x) = Just (SomeField x)
toField (Map _ x)  = toField x
toField _          = Nothing

queryField :: Path
           -> Form m v a
           -> (forall b. Field v b -> Maybe c)
           -> Maybe c
queryField path form f = do
    SomeForm form'  <- listToMaybe $ lookupForm path form
    SomeField field <- toField form'
    f field

ann :: Path -> Result v a -> Result [(Path, v)] a
ann _    (Success x) = Success x
ann path (Error x)   = Error [(path, x)]

eval :: Monad m => Method -> Env m -> Form m v a
     -> m (Result [(Path, v)] a, [(Path, FormInput)])
eval = eval' []

eval' :: Monad m => Path -> Method -> Env m -> Form m v a
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
