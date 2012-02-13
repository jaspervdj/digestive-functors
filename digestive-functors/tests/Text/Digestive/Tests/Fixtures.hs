{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Tests.Fixtures
    ( Type (..)
    , Pokemon (..)
    , pokemonForm
    ) where

import Control.Applicative ((<$>), (<*>))

import Data.Text (Text)

import Text.Digestive.Form

data Type = Water | Fire | Leaf
    deriving (Eq, Show)

typeForm :: Monad m => Form m Text Type
typeForm = choice [(Water, "Water"), (Fire, "Fire"), (Leaf, "Leaf")] Nothing

data Pokemon = Pokemon
    { pokemonName  :: Text
    , pokemonLevel :: Int
    , pokemonType  :: Type
    , pokemonRare  :: Bool
    } deriving (Eq, Show)

levelForm :: Monad m => Form m Text Int
levelForm =
    check "Level cannot be higher than 99" (<= 99) $
    check "Level should be at least 1"     (> 1)   $
    stringRead "Cannot parse level" (Just 5)

pokemonForm :: Monad m => Form m Text Pokemon
pokemonForm = Pokemon
    <$> "name"  .: text Nothing
    <*> "level" .: levelForm
    <*> "type"  .: typeForm
    <*> "rare"  .: bool False
