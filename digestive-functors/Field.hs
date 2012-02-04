{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings #-}
import Data.Monoid (Monoid, mappend, mempty)
import Control.Applicative (Applicative (..), (<$>))
import Data.Maybe (fromMaybe, maybeToList)

import Data.Text (Text)
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

--------------------------------------------------------------------------------

type Ref = Maybe Text
type Path = [Text]

data Form i v a where
    Pure :: Ref -> Field v a -> Form i v a
    App  :: Ref -> Form i v (b -> a) -> Form i v b -> Form i v a

    Map  :: (b -> Result v a) -> Form i v b -> Form i v a

instance Monoid v => Functor (Form i v) where
    fmap = Map . (pure .)

instance Monoid v => Applicative (Form i v) where
    pure x  = Pure Nothing (Singleton x)
    x <*> y = App Nothing x y

instance Show (Form i v a) where
    show = unlines . showForm

showForm :: Form i v a -> [String]
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

ref :: Text -> Form i v a -> Form i v a
ref r (Pure _ x)  = Pure (Just r) x
ref r (App _ x y) = App (Just r) x y
ref r (Map f x)   = Map f (ref r x)

getRef :: Form i v a -> Ref
getRef (Pure r _)  = r
getRef (App r _ _) = r
getRef (Map _ x)   = getRef x

transform :: (a -> Result v b) -> Form i v a -> Form i v b
transform = Map

--------------------------------------------------------------------------------

type AnnResult v a = Result [(Path, v)] a

ann :: Path -> Result v a -> AnnResult v a
ann _    (Success x) = Success x
ann path (Error x)   = Error [(path, x)]

--------------------------------------------------------------------------------

type Env = [(Path, Text)]  -- Lol

eval :: Monoid v => Env -> Form i v a -> AnnResult v a
eval = eval' []

eval' :: Monoid v => Path -> Env -> Form i v a -> AnnResult v a

eval' context env form = case form of

    Pure (Just _) field ->
        evalField (lookup path env) field

    App r x y ->
        let x' = eval' path env x
            y' = eval' path env y
        in  x' <*> y'

    Map f x ->
        eval' context env x >>= ann path . f

  where
    path = context ++ maybeToList (getRef form)

evalField :: Monoid v => Maybe Text -> Field v a -> AnnResult v a
evalField _        (Singleton x) = pure x
evalField Nothing  (Text x)      = pure x
evalField (Just x) (Text _)      = pure x
evalField Nothing  (Choice ls x) = pure $ fst $ ls !! x
evalField (Just x) (Choice ls _) = pure $
    fst $ ls !! read (T.unpack x)  -- fix

--------------------------------------------------------------------------------

data User = User Text Int
    deriving (Show)

userForm :: Form i Text User
userForm = User
    <$> ref "name" (text Nothing)
    <*> ref "age" (stringRead (Just 21))

pairForm :: Form i Text (User, User)
pairForm = (,)
    <$> ref "fst" userForm
    <*> ref "snd" userForm

--------------------------------------------------------------------------------

text :: Maybe Text -> Form i v Text
text def = Pure Nothing $ Text $ fromMaybe "" def

string :: Monoid v => Maybe String -> Form i v String
string = fmap T.unpack . text . fmap T.pack

stringRead :: (Monoid v, Read a, Show a) => Maybe a -> Form i v a
stringRead = transform readTransform . string . fmap show
  where
    readTransform str = case readMaybe str of
        Just x  -> return x
        Nothing -> Error mempty

readMaybe :: Read a => String -> Maybe a
readMaybe str = case readsPrec 1 str of
    [(x, "")] -> Just x
    _         -> Nothing
