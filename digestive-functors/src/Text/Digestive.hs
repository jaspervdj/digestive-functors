{-# LANGUAGE GADTs #-}
module Text.Digestive
    (
    ) where

import Control.Applicative (Applicative (..), (<$>))
import Data.Map (Map)
import Data.Monoid (Monoid, mappend, mempty)
import qualified Data.Map as M

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

--------------------------------------------------------------------------------

newtype Validate i v a = Validate {unValidate :: i -> Result v a}

instance Functor (Validate i v) where
    fmap f (Validate x) = Validate (fmap f . x)

instance Monoid v => Applicative (Validate i v) where
    pure x                    = Validate $ const $ pure x
    Validate x <*> Validate y = Validate $ \i -> x i <*> y i

--------------------------------------------------------------------------------

data Tree i a where
    Pure :: i -> a -> Tree i a
    App  :: i -> Tree i (b -> a) -> Tree i b -> Tree i a

instance Functor (Tree i) where
    fmap f (Pure i x)  = Pure i (f x)
    fmap f (App i x y) = App i (fmap (f .) x) y

instance Monoid i => Applicative (Tree i) where
    pure x  = Pure mempty x
    x <*> y = App mempty x y

showTree :: Show i => Tree i a -> [String]
showTree (Pure i x)  = ["Pure " ++ show i ++ " _"]
showTree (App i x y) = concat
    [ ["App " ++ show i]
    , map indent (showTree x)
    , map indent (showTree y)
    ]
  where
    indent = ("  " ++)

decorate :: (i -> i) -> Tree i a -> Tree i a
decorate f (Pure i x)  = Pure (f i) x
decorate f (App i x y) = App (f i) x y

--------------------------------------------------------------------------------

type Ref = [String]

newtype Form i v a = Form {unForm :: Tree Ref (Validate i v a)}

instance Functor (Form i v) where
    fmap f = Form . fmap (fmap f) . unForm

instance Monoid v => Applicative (Form i v) where
    pure x            = Form $ pure $ pure x
    Form x <*> Form y = Form $ (<*>) <$> x <*> y

showForm :: Show i => Form i v a -> [String]
showForm = showTree . unForm

printForm :: Show i => Form i v a -> IO ()
printForm = mapM_ putStrLn . showForm

ref :: String -> Form i v a -> Form i v a
ref r = Form . decorate (const [r]) . unForm

--------------------------------------------------------------------------------

eval :: i -> Form i v a -> Either () a
eval = undefined

--------------------------------------------------------------------------------

data User = User String Int
    deriving (Show)

userForm :: Form i String User
userForm = User
    <$> ref "name" (pure "jasper")
    <*> ref "age" (pure 21)
