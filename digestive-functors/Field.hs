{-# LANGUAGE ExistentialQuantification, GADTs #-}
import Data.Monoid (Monoid, mappend, mempty)
import Control.Applicative (Applicative (..), (<$>))

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

--------------------------------------------------------------------------------

data Field v a where
    Singleton :: a -> Field v a
    Text      :: Text -> Field v Text
    Choice    :: [(a, v)] -> Int -> Field v a

data SomeField v = forall a. SomeField (Field v a)

-- LONG LIVE GADTs!
printSomeField :: SomeField v -> IO ()
printSomeField (SomeField f) = case f of
    Text t -> putStrLn (T.unpack t)
    _      -> putStrLn "can't print and shit"

--------------------------------------------------------------------------------

type Ref = Maybe String

data Form i v a where
    Pure :: Ref -> Field v a -> Form i v a
    App  :: Ref -> Form i v (b -> a) -> Form i v b -> Form i v a

    Map  :: (b -> Result v a) -> Form i v b -> Form i v a

--------------------------------------------------------------------------------

instance Monoid v => Functor (Form i v) where
    fmap = Map . (pure .)

instance Monoid v => Applicative (Form i v) where
    pure x  = Pure Nothing (Singleton x)
    x <*> y = App Nothing x y
