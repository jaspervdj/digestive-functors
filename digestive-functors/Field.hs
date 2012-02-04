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
ref _ f           = f

--------------------------------------------------------------------------------

type Env = [([Text], Text)]  -- Lol

eval :: Monoid v => Env -> Form i v a -> Result v a
eval = eval' []

eval' :: Monoid v => [Text] -> Env -> Form i v a -> Result v a

eval' context env (Pure (Just r) field) =
    let ref = context ++ [r]
    in  evalField (lookup ref env) field

eval' context env (App r x y) =
    let ref = context ++ maybeToList r
        x'  = eval' ref env x
        y'  = eval' ref env y
    in  x' <*> y'

eval' context env (Map f x) =
    eval' context env x >>= f

evalField :: Monoid v => Maybe Text -> Field v a -> Result v a
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
    <*> ref "age" (pure 21)

pairForm :: Form i Text (User, User)
pairForm = (,)
    <$> ref "fst" userForm
    <*> ref "snd" userForm

--------------------------------------------------------------------------------

text :: Maybe Text -> Form i v Text
text def = Pure Nothing $ Text $ fromMaybe "" def
