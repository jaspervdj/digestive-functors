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

-- | A single input field. This usually maps to a single HTML @<input>@ element.
data Field v a where
    Singleton :: a -> Field v a
    Text      :: Text -> Field v Text
    Choice    :: Eq a => [(a, v)] -> Int -> Field v a
    Bool      :: Bool -> Field v Bool
    File      :: Field v (Maybe FilePath)

instance Show (Field v a) where
    show (Singleton _) = "Singleton _"
    show (Text t)      = "Text " ++ show t
    show (Choice _ _)  = "Choice _ _"
    show (Bool b)      = "Bool " ++ show b
    show (File)        = "File"

data SomeField v = forall a. SomeField (Field v a)

evalField :: Method       -- ^ Get/Post
          -> [FormInput]  -- ^ Given input
          -> Field v a    -- ^ Field
          -> a            -- ^ Result
evalField _    _                 (Singleton x) = x
evalField _    (TextInput x : _) (Text _)      = x
evalField _    _                 (Text x)      = x
evalField _    (TextInput x : _) (Choice ls y) = fromMaybe (fst $ ls !! y) $ do
    -- Expects input in the form of @foo.bar.2@
    t <- listToMaybe $ reverse $ toPath x
    i <- readMaybe $ T.unpack t
    return $ fst $ ls !! i
evalField _    _                 (Choice ls x) = fst $ ls !! x
evalField Get  _                 (Bool x)      = x
evalField Post (TextInput x : _) (Bool _)      = x == "on"
evalField Post _                 (Bool _)      = False
evalField Post (FileInput x : _) File          = Just x
evalField _    _                 File          = Nothing
