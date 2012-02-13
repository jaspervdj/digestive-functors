{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings #-}
module Text.Digestive.Field
    ( Field (..)
    , SomeField (..)
    , evalField
    ) where

import Data.Maybe (fromMaybe, listToMaybe)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Digestive.Types
import Text.Digestive.Util

data Field v a where
    Singleton :: a -> Field v a
    Text      :: Text -> Field v Text
    Choice    :: [(a, v)] -> Int -> Field v a
    Bool      :: Bool -> Field v Bool

instance Show (Field v a) where
    show (Singleton _) = "Singleton _"
    show (Text t)      = "Text " ++ show t
    show (Choice _ _)  = "Choice _ _"
    show (Bool b)      = "Bool " ++ show b

data SomeField v = forall a. SomeField (Field v a)

evalField :: Maybe Text -> Field v a -> a
evalField _        (Singleton x) = x
evalField Nothing  (Text x)      = x
evalField (Just x) (Text _)      = x
evalField Nothing  (Choice ls x) = fst $ ls !! x
evalField (Just x) (Choice ls y) = fromMaybe (fst $ ls !! y) $ do
    -- Expects input in the form of @foo.bar.2@
    t <- listToMaybe $ reverse $ toPath x
    i <- readMaybe $ T.unpack t
    return $ fst $ ls !! i
evalField Nothing  (Bool x)      = x
evalField (Just x) (Bool _)      = x == "on"
