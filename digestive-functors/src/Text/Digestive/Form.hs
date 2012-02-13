{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings, Rank2Types #-}
module Text.Digestive.Form
    ( Form
    , SomeForm (..)
    , (.:)
    , lookupForm
    , toField
    , queryField
    , eval

      -- * Forms
    , text
    , string
    , stringRead
    , choice
    , bool

      -- * Validation
    , check
    ) where

import Control.Applicative (Applicative (..))
import Data.List (findIndex)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Monoid (Monoid)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Digestive.Types
import Text.Digestive.Field
import Text.Digestive.Util

type Ref = Maybe Text

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

transform :: Monad m => (a -> m (Result v b)) -> Form m v a -> Form m v b
transform f (Map g x) = flip Map x $ \y -> do
    y' <- g y
    case y' of
        Error errs  -> return $ Error errs
        Success y'' -> f y''
transform f x         = Map f x

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

--------------------------------------------------------------------------------

ann :: Path -> Result v a -> Result [(Path, v)] a
ann _    (Success x) = Success x
ann path (Error x)   = Error [(path, x)]

--------------------------------------------------------------------------------

eval :: Monad m => Method -> Env m -> Form m v a
     -> m (Result [(Path, v)] a, [(Path, Text)])
eval = eval' []

eval' :: Monad m => Path -> Method -> Env m -> Form m v a
      -> m (Result [(Path, v)] a, [(Path, Text)])

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
        case x' of
            Success x'' -> do
                x''' <- f x''  -- This is a bit ridiculous
                return (ann path x''', inp)
            Error errs  -> return (Error errs, inp)

  where
    path = context ++ maybeToList (getRef form)


text :: Maybe Text -> Form m v Text
text def = Pure Nothing $ Text $ fromMaybe "" def

string :: Monad m => Maybe String -> Form m v String
string = fmap T.unpack . text . fmap T.pack

stringRead :: (Monad m, Read a, Show a) => v -> Maybe a -> Form m v a
stringRead err = transform readTransform . string . fmap show
  where
    readTransform = return . maybe (Error err) return . readMaybe

choice :: Eq a => [(a, v)] -> Maybe a -> Form m v a
choice items def = Pure Nothing $ Choice items $ fromMaybe 0 $
    maybe Nothing (\d -> findIndex ((== d) . fst) items) def

bool :: Bool -> Form m v Bool
bool = Pure Nothing . Bool

check :: Monad m => v -> (a -> Bool) -> Form m v a -> Form m v a
check err predicate form = transform f form
  where
    f x | predicate x = return (return x)
        | otherwise   = return (Error err)
