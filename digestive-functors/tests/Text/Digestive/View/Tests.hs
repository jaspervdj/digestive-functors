{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.View.Tests
    ( tests
    ) where

import Control.Monad.Identity (runIdentity)

import Data.Text (Text)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

import Text.Digestive.Tests.Fixtures
import Text.Digestive.Types
import Text.Digestive.View

tests :: Test
tests = testGroup "Text.Digestive.View.Tests"
    [ testCase "Simple postForm" $ (@=?)
        (Pokemon "charmander" 5 Fire False) $
        fromRight $ runIdentity $ postForm pokemonForm $ testEnv
            [ ("name",  "charmander")
            , ("level", "5")
            , ("type",  "type.1")
            ]

    , testCase "Nested postForm" $ (@=?)
        (Catch (Pokemon "charmander" 5 Fire False) Ultra) $
        fromRight $ runIdentity $ postForm catchForm $ testEnv
            [ ("pokemon.name",  "charmander")
            , ("pokemon.level", "5")
            , ("pokemon.type",  "type.1")
            , ("ball",          "ball.2")
            ]

    , testCase "subView errors" $ (@=?)
        ["Cannot parse level"] $
        errors "level" $ subView "pokemon" $ fromLeft $ runIdentity $
            postForm catchForm $ testEnv [("pokemon.level", "hah.")]
    ]

testEnv :: Monad m => [(Text, Text)] -> Env m
testEnv input key = return $ lookup (fromPath key) input

fromLeft :: Either a b -> a
fromLeft (Left x)  = x
fromLeft (Right _) = error "fromLeft (Right _)"

fromRight :: Either a b -> b
fromRight (Left _)  = error "fromRight (Left _)"
fromRight (Right x) = x
