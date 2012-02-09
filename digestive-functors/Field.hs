{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings, Rank2Types #-}
module Field where

import Control.Applicative (Applicative (..), (<$>))
import Control.Arrow (first)
import Control.Monad ((<=<))
import Data.List (findIndex)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Monoid (Monoid, mappend, mempty)

import GHC.Exts (IsString)
import Data.Text (Text)
import Text.Blaze (Html)
import qualified Data.Text as T

--------------------------------------------------------------------------------

data Result v a
    = Success a
    | Error v
    deriving (Show)

instance Functor (Result v) where
    fmap f (Success x) = Success (f x)
    fmap _ (Error x)   = Error x

instance Monoid v => Applicative (Result v) where
    pure x                  = Success x
    Error x   <*> Error y   = Error $ mappend x y
    Error x   <*> Success _ = Error x
    Success _ <*> Error y   = Error y
    Success x <*> Success y = Success (x y)

instance Monad (Result v) where
    return x          = Success x
    (Error x)   >>= _ = Error x
    (Success x) >>= f = f x

--------------------------------------------------------------------------------

data Field v a where
    Singleton :: a -> Field v a
    Text      :: Text -> Field v Text
    Choice    :: [(a, v)] -> Int -> Field v a

instance Show (Field v a) where
    show (Singleton _) = "Singleton _"
    show (Text t)      = "Text " ++ show t
    show (Choice _ _)  = "Choice _ _"

data SomeField v = forall a. SomeField (Field v a)

-- LONG LIVE GADTs!
printSomeField :: SomeField v -> IO ()
printSomeField (SomeField f) = case f of
    Text t -> putStrLn (T.unpack t)
    _      -> putStrLn "can't print and shit"

fieldDefaultInput :: Field v a -> Maybe Text
fieldDefaultInput (Text t) = Just t
fieldDefaultInput _        = Nothing

--------------------------------------------------------------------------------

type Ref = Maybe Text
type Path = [Text]

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

ref :: Text -> Form m v a -> Form m v a
ref r (Pure _ x)  = Pure (Just r) x
ref r (App _ x y) = App (Just r) x y
ref r (Map f x)   = Map f (ref r x)

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
            | r == r' && null rs -> [SomeForm form]
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

{-
formDefaultInput :: Form v a -> Maybe Text
formDefaultInput form = do
    someField <- toField form
    case someField of
        SomeField field -> fieldDefaultInput field
-}

--------------------------------------------------------------------------------

ann :: Path -> Result v a -> Result [(Path, v)] a
ann _    (Success x) = Success x
ann path (Error x)   = Error [(path, x)]

--------------------------------------------------------------------------------

type Env = [(Path, Text)]  -- Lol

eval :: Monad m => Env -> Form m v a -> m (Result [(Path, v)] a)
eval = eval' []

eval' :: Monad m => Path -> Env -> Form m v a -> m (Result [(Path, v)] a)

eval' context env form = case form of

    Pure (Just _) field ->
        return $ evalField (lookup path env) field

    App r x y -> do
        x' <- eval' path env x
        y' <- eval' path env y
        return $ x' <*> y'

    Map f x -> do
        x' <- eval' context env x
        case x' of
            Success x'' -> f x'' >>= return . ann path
            Error errs  -> return $ Error errs

  where
    path = context ++ maybeToList (getRef form)

evalField :: Maybe Text -> Field v a -> Result [(Path, v)] a
evalField _        (Singleton x) = pure x
evalField Nothing  (Text x)      = pure x
evalField (Just x) (Text _)      = pure x
evalField Nothing  (Choice ls x) = pure $ fst $ ls !! x
evalField (Just x) (Choice ls _) = pure $
    fst $ ls !! read (T.unpack x)  -- fix

--------------------------------------------------------------------------------

data View m v a = View
    { viewForm   :: Form m v a
    , viewInput  :: [(Path, Text)]
    , viewErrors :: [(Path, v)]
    } deriving (Show)

getForm :: Form m v a -> View m v a
getForm form = View form [] []

postForm :: Monad m => Form m v a -> Env -> m (Either (View m v a) a)
postForm form env = eval env form >>= \r -> return $ case r of
    Error errs -> Left $ View form env errs
    Success x  -> Right x

--------------------------------------------------------------------------------

data User = User Text Int Sex
    deriving (Show)

data Sex = Female | Male
    deriving (Eq, Show)

userForm :: Monad m => Form m Html User
userForm = User
    <$> ref "name" (text (Just "jasper"))
    <*> ref "age" (stringRead (Just 21))
    <*> ref "sex" (choice [(Female, "female"), (Male, "male")] (Just Male))

pairForm :: Monad m => Form m Html (User, User)
pairForm = (,)
    <$> ref "fst" userForm
    <*> ref "snd" userForm

--------------------------------------------------------------------------------

text :: Maybe Text -> Form m v Text
text def = Pure Nothing $ Text $ fromMaybe "" def

string :: Monad m => Maybe String -> Form m v String
string = fmap T.unpack . text . fmap T.pack

stringRead :: (IsString s, Monad m, Read a, Show a) => Maybe a -> Form m s a
stringRead = transform readTransform . string . fmap show
  where
    readTransform str = return $ case readMaybe str of
        Just x  -> return x
        Nothing -> Error "PBKAC"

choice :: Eq a => [(a, v)] -> Maybe a -> Form m v a
choice items def = Pure Nothing $ Choice items $ fromMaybe 0 $
    maybe Nothing (\d -> findIndex ((== d) . fst) items) def

readMaybe :: Read a => String -> Maybe a
readMaybe str = case readsPrec 1 str of
    [(x, "")] -> Just x
    _         -> Nothing


--------------------------------------------------------------------------------

toPath :: Text -> Path
toPath = T.split (== '.')
