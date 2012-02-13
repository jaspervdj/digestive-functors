{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Tests.Fixtures
    ( Type (..)
    , Pokemon (..)
    , pokemonForm
    , Ball (..)
    , ballForm
    , Catch (..)
    , catchForm
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

data Ball = Poke | Great | Ultra | Master
    deriving (Eq, Show)

ballForm :: Monad m => Form m Text Ball
ballForm = choice
    [(Poke, "Poke"), (Great, "Great"), (Ultra, "Ultra"), (Master, "Master")]
    Nothing

data Catch = Catch
    { catchPokemon :: Pokemon
    , catchBall    :: Ball
    } deriving (Eq, Show)

catchForm :: Monad m => Form m Text Catch
catchForm = check "You need a better ball" canCatch $ Catch
    <$> "pokemon" .: pokemonForm
    <*> "ball"    .: ballForm

canCatch :: Catch -> Bool
canCatch (Catch (Pokemon _ _ _ False) _)      = True
canCatch (Catch (Pokemon _ _ _ True)  Ultra)  = True
canCatch (Catch (Pokemon _ _ _ True)  Master) = True
canCatch _                                    = False
