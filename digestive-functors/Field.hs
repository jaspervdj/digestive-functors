{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings #-}
import Control.Applicative (Applicative (..), (<$>))
import Control.Arrow (first)
import Data.List (findIndex)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid (Monoid, mappend, mempty)

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

instance Functor (Form i v) where
    fmap = Map . (return .)

instance Monoid v => Applicative (Form i v) where
    pure x  = Pure Nothing (Singleton x)
    x <*> y = App Nothing x y

instance Show (Form i v a) where
    show = unlines . showForm

data SomeForm i v = forall a. SomeForm (Form i v a)

instance Show (SomeForm i v) where
    show (SomeForm f) = show f

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

lookupForm :: Path -> Form i v a -> [SomeForm i v]
lookupForm []       form = [SomeForm form]
lookupForm [r]      form = case getRef form of
    Just r' | r == r' -> [SomeForm form]
    _                 -> []
lookupForm (r : rs) form = case form of
    Pure _ _        -> []  -- Path goes deeper than what we have
    App Nothing x y -> lookupForm (r : rs) x ++ lookupForm (r : rs) y
    App (Just r') x y
        | r == r'   -> lookupForm rs x ++ lookupForm rs y
        | otherwise -> []
    Map _ x         -> lookupForm (r : rs) x

--------------------------------------------------------------------------------

type AnnResult v a = Result [(Path, v)] a

ann :: Path -> Result v a -> AnnResult v a
ann _    (Success x) = Success x
ann path (Error x)   = Error [(path, x)]

--------------------------------------------------------------------------------

type Env = [(Path, Text)]  -- Lol

eval :: Env -> Form i v a -> AnnResult v a
eval = eval' []

eval' :: Path -> Env -> Form i v a -> AnnResult v a

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

evalField :: Maybe Text -> Field v a -> AnnResult v a
evalField _        (Singleton x) = pure x
evalField Nothing  (Text x)      = pure x
evalField (Just x) (Text _)      = pure x
evalField Nothing  (Choice ls x) = pure $ fst $ ls !! x
evalField (Just x) (Choice ls _) = pure $
    fst $ ls !! read (T.unpack x)  -- fix

--------------------------------------------------------------------------------

data User = User Text Int Sex
    deriving (Show)

data Sex = Female | Male
    deriving (Eq, Show)

userForm :: Form i Text User
userForm = User
    <$> ref "name" (text Nothing)
    <*> ref "age" (stringRead (Just 21))
    <*> ref "sex" (choice [(Female, "female"), (Male, "male")] Nothing)

pairForm :: Form i Text (User, User)
pairForm = (,)
    <$> ref "fst" userForm
    <*> ref "snd" userForm

--------------------------------------------------------------------------------

text :: Maybe Text -> Form i v Text
text def = Pure Nothing $ Text $ fromMaybe "" def

string :: Maybe String -> Form i v String
string = fmap T.unpack . text . fmap T.pack

stringRead :: (Read a, Show a) => Maybe a -> Form i Text a
stringRead = transform readTransform . string . fmap show
  where
    readTransform str = case readMaybe str of
        Just x  -> return x
        Nothing -> Error "PBKAC"

choice :: Eq a => [(a, v)] -> Maybe a -> Form i v a
choice items def = Pure Nothing $ Choice items $ fromMaybe 0 $
    maybe Nothing (\d -> findIndex ((== d) . fst) items) def

readMaybe :: Read a => String -> Maybe a
readMaybe str = case readsPrec 1 str of
    [(x, "")] -> Just x
    _         -> Nothing

--------------------------------------------------------------------------------

test :: Form i v a -> [(Text, Text)] -> AnnResult v a
test form env = eval (map (first $ T.split (== '.')) env) form

--------------------------------------------------------------------------------

test01 = test pairForm
    [ ("fst.name", "Laurel")
    , ("fst.age", "28")
    , ("fst.sex", "1")
    , ("snd.name", "Hardy")
    , ("snd.age", "26")
    , ("snd.sex", "1")
    ]

test02 = test pairForm
    [ ("fst.name", "Laurel")
    , ("fst.age", "28")
    , ("snd.name", "Hardy")
    , ("snd.age", "foo")
    ]
