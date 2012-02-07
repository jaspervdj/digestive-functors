{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings #-}
import Control.Applicative (Applicative (..), (<$>))
import Control.Arrow (first)
import Control.Monad ((<=<))
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

data Tree i v a where
    Pure :: i -> Field v a -> Tree i v a
    App  :: i -> Tree i v (b -> a) -> Tree i v b -> Tree i v a

    Map  :: (b -> Result v a) -> Tree i v b -> Tree i v a

instance Functor (Tree i v) where
    fmap = Map . (return .)

instance (Monoid i, Monoid v) => Applicative (Tree i v) where
    pure x  = Pure mempty (Singleton x)
    x <*> y = App mempty x y

instance Show i => Show (Tree i v a) where
    show = unlines . showTree

data SomeTree i v = forall a. SomeTree (Tree i v a)

instance Show i => Show (SomeTree i v) where
    show (SomeTree f) = show f

showTree :: Show i => Tree i v a -> [String]
showTree tree = case tree of
    (Pure i x)  -> ["Pure (" ++ show i ++ ") (" ++ show x ++ ")"]
    (App i x y) -> concat
        [ ["App (" ++ show i ++ ")"]
        , map indent (showTree x)
        , map indent (showTree y)
        ]
    (Map _ x)   -> "Map _" : map indent (showTree x)
  where
    indent = ("  " ++)

transform :: (a -> Result v b) -> Tree i v a -> Tree i v b
transform f (Map g x) = Map (f <=< g) x  -- Optimization
transform f x         = Map f x

annotate :: i -> Tree i v a -> Tree i v a
annotate i (Pure _ x)  = Pure i x
annotate i (App _ x y) = App i x y
annotate i (Map f x)   = Map f (annotate i x)

annotation :: Tree i v a -> i
annotation (Pure i _)  = i
annotation (App i _ _) = i
annotation (Map _ x)   = annotation x

children :: Tree i v a -> [SomeTree i v]
children (Pure _ _)  = []
children (App _ x y) = [SomeTree x, SomeTree y]
children (Map _ x)   = children x

--------------------------------------------------------------------------------

type Ref = Maybe Text
type Path = [Text]
type Form = Tree Ref

ref :: Text -> Form v a -> Form v a
ref = annotate . Just

getRef :: Form v a -> Ref
getRef = annotation

lookupForm :: Path -> Form v a -> [SomeTree Ref v]
lookupForm path = go path . SomeTree
  where
    go []       tree            = [tree]
    go (r : rs) (SomeTree tree) = case getRef tree of
        Just r'
            | r == r' && null rs -> [SomeTree tree]
            | r == r'            -> children tree >>= go rs
            | otherwise          -> []
        Nothing                  -> children tree >>= go (r : rs)

--------------------------------------------------------------------------------

type AnnResult v a = Result [(Path, v)] a

ann :: Path -> Result v a -> AnnResult v a
ann _    (Success x) = Success x
ann path (Error x)   = Error [(path, x)]

--------------------------------------------------------------------------------

type Env = [(Path, Text)]  -- Lol

eval :: Env -> Form v a -> AnnResult v a
eval = eval' []

eval' :: Path -> Env -> Form v a -> AnnResult v a

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

userForm :: Form Text User
userForm = User
    <$> ref "name" (text Nothing)
    <*> ref "age" (stringRead (Just 21))
    <*> ref "sex" (choice [(Female, "female"), (Male, "male")] Nothing)

pairForm :: Form Text (User, User)
pairForm = (,)
    <$> ref "fst" userForm
    <*> ref "snd" userForm

--------------------------------------------------------------------------------

text :: Maybe Text -> Form v Text
text def = Pure Nothing $ Text $ fromMaybe "" def

string :: Maybe String -> Form v String
string = fmap T.unpack . text . fmap T.pack

stringRead :: (Read a, Show a) => Maybe a -> Form Text a
stringRead = transform readTransform . string . fmap show
  where
    readTransform str = case readMaybe str of
        Just x  -> return x
        Nothing -> Error "PBKAC"

choice :: Eq a => [(a, v)] -> Maybe a -> Form v a
choice items def = Pure Nothing $ Choice items $ fromMaybe 0 $
    maybe Nothing (\d -> findIndex ((== d) . fst) items) def

readMaybe :: Read a => String -> Maybe a
readMaybe str = case readsPrec 1 str of
    [(x, "")] -> Just x
    _         -> Nothing

--------------------------------------------------------------------------------

test :: Form v a -> [(Text, Text)] -> AnnResult v a
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
