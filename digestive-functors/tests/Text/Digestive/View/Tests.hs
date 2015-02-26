-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Digestive.View.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Exception              (SomeException, handle)
import           Control.Monad.Identity         (runIdentity)
import           Data.Text                      (Text)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@=?), (@?=))
import qualified Test.HUnit                     as H


--------------------------------------------------------------------------------
import           Text.Digestive.Form.Encoding
import           Text.Digestive.Tests.Fixtures
import           Text.Digestive.Types
import           Text.Digestive.View


--------------------------------------------------------------------------------
assertError :: Show a => a -> H.Assertion
assertError x = handle (\(_ :: SomeException) -> H.assert True) $
    x `seq` H.assertFailure $ "Should throw an error but gave: " ++ show x


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Text.Digestive.View.Tests"
    [ testCase "Simple postForm" $ (@=?)
        (Just (Pokemon "charmander" (Just 5) Fire [Fire, Leaf] False)) $
        snd $ runTrainerM $ postForm "f" pokemonForm $ testEnv
            [ ("f.name",  "charmander")
            , ("f.level", "5")
            , ("f.type",  "type.1")
            , ("f.types",  "types.1")
            , ("f.types",  "types.2")
            ]

    , testCase "optional unspecified" $ (@=?)
        (Just (Pokemon "magmar" Nothing Fire [] False)) $
        snd $ runTrainerM $ postForm "f" pokemonForm $ testEnv
            [ ("f.name",  "magmar")
            , ("f.type",  "type.1")
            ]

    , testCase "stringRead float" $ (@=?)
        (Just 4.323 :: Maybe Float) $
        snd $ runIdentity $ postForm "f" floatForm $ testEnv
            [("f.f", "4.323")]

    , testCase "validateOptional validated successfully" $ (@=?)
        (Just (ValidateOptionalData (Just 8))) $
        snd $ runIdentity $ postForm "f" validateOptionalForm $ testEnv
            [("f.first_field", "8")]

    , testCase "validateOptional allows nothing" $ (@=?)
        (Just (ValidateOptionalData Nothing)) $
        snd $ runIdentity $ postForm "f" validateOptionalForm $ testEnv
            [("f.first_field", "")]

    , testCase "validateOptional fails for invalid data" $ (@=?)
        ["input must be even"] $
        childErrors "" $ fst $ runIdentity $ postForm "f" validateOptionalForm $ testEnv
            [("f.first_field", "9")]

    , testCase "conditions allows for multiple independent failing validations" $ (@=?)
        [["input must be even", "input is too small"]] $
        childErrors "" $ fst $ runIdentity $ postForm "f" independentValidationsForm $ testEnv
            [("f.first_field", "9")]

    , testCase "Failing checkM" $ (@=?)
        ["This pokemon will not obey you!"] $
        childErrors "" $ fst $ runTrainerM $ postForm "f" pokemonForm $ testEnv
            [ ("f.name",  "charmander")
            , ("f.level", "9000")
            , ("f.type",  "type.1")
            ]

    , testCase "Failing validate" $ (@=?)
        ["dog is not a pokemon!"] $
        childErrors "" $ fst $ runTrainerM $ postForm "f" pokemonForm $ testEnv
            [("f.name", "dog")]

    , testCase "Simple fieldInputChoice" $ (@=?)
        "Leaf" $
        snd $ selection $ fieldInputChoice "type" $ fst $ runTrainerM $
            postForm "f" pokemonForm $ testEnv [("f.type",  "type.2")]

    , testCase "Simple fieldInputChoices" $ (@=?)
        ["Fire", "Leaf"] $
        map snd $ selections $ fieldInputChoices "types" $ fst $ runTrainerM $
            postForm "f" pokemonForm $ testEnv [("f.types",  "types.1"), ("f.types",  "types.2")]

    , testCase "Nested postForm" $ (@=?)
        (Just (Catch (Pokemon "charmander" (Just 5) Fire [] False) Ultra)) $
        snd $ runTrainerM $ postForm "f" catchForm $ testEnv
            [ ("f.pokemon.name",  "charmander")
            , ("f.pokemon.level", "5")
            , ("f.pokemon.type",  "type.1")
            , ("f.ball",          "ball.2")
            ]

    , testCase "subView errors" $ (@=?)
        ["Cannot parse level"] $
        errors "level" $ subView "pokemon" $ fst $ runTrainerM $
            postForm "f" catchForm $ testEnv [("f.pokemon.level", "hah.")]

    , testCase "subView childErrors" $ (@=?)
        ["Cannot parse level"] $
        childErrors "" $ subView "pokemon" $ fst $ runTrainerM $
            postForm "f" catchForm $ testEnv [("f.pokemon.level", "hah.")]

    , testCase "subView input" $ (@=?)
        "Leaf" $
        snd $ selection $ fieldInputChoice "type" $ subView "pokemon" $ fst $
            runTrainerM $ postForm "f" catchForm $ testEnv
                [ ("f.pokemon.level", "hah.")
                , ("f.pokemon.type",  "type.2")
                ]

    , testCase "subView input choices" $ (@=?)
        ["Fire", "Leaf"] $
        map snd $ selections $ fieldInputChoices "types" $ subView "pokemon" $ fst $
            runTrainerM $ postForm "f" catchForm $ testEnv
                [ ("f.pokemon.level", "hah.")
                , ("f.pokemon.types",  "types.1")
                , ("f.pokemon.types",  "types.2")
                ]

    , testCase "subViews length" $ (@=?)
        5 $
        length $ subViews $ runTrainerM $ getForm "f" pokemonForm

    , testCase "subViews after subView length" $ (@=?)
        5 $
        length $ subViews $ subView "pokemon" $
            runTrainerM $ getForm "f" catchForm

    , testCase "Abusing Choice as Text" $ assertError $
        fieldInputText "type" $ runTrainerM $ getForm "f" pokemonForm

    , testCase "Abusing Bool as Choice" $ assertError $
        fieldInputChoice "rare" $ runTrainerM $ getForm "f" pokemonForm

    , testCase "Abusing Text as Bool" $ assertError $
        fieldInputBool "name" $ runTrainerM $ getForm "f" pokemonForm

    , testCase "monadic choiceWith" $ (@=?)
        (Just (Order comet 2)) $
        snd $ runDatabase $ postForm "f" (orderForm Nothing) $ testEnv
            -- We actually need f.product.cm_gs for the choice input, but this
            -- must work as well!
            [ ("f.product",  "cm_gs")
            , ("f.quantity", "2")
            ]

    , testCase "monadic view query" $ (@=?)
        "Earthwing Belly Racer" $
        snd $ selection $ fieldInputChoice "product" $ runDatabase $
                getForm "f"
                -- With a default
                (orderForm $ Just $ Order earthwing 10)

    , -- Let me just order 3 awesome skateboards here
      testCase "Simple listOf" $ do
        let (view, result) = runDatabase $ postForm "f" (ordersForm Nothing) $
                                testEnv
                                    [ ("f.name",               "Jasper")
              {-   \    /\    -}    , ("f.orders.indices",     "0,10")
              {-    )  ( ')   -}    , ("f.orders.0.product",   "cm_gs")
              {-   (  /  )    -}    , ("f.orders.0.quantity",  "2")
              {-    \(__)|    -}    , ("f.orders.10.product",  "s9_ao")
                                    , ("f.orders.10.quantity", "1")
                                    ]

        result @?= Just
            ( "Jasper"
            , [ Order (Product "cm_gs" "Comet Grease Shark") 2
              , Order (Product "s9_ao" "Sector 9 Agent Orange") 1
              ]
            )

        let subViews' = listSubViews "orders" view
        fieldInputText "quantity" (head subViews') @?= "2"

    , testCase "listOf with defaults" $ do
        let view = runDatabase $ getForm "f" $ ordersForm $ Just
                        ( "Jasper"
                        , [ Order comet 2
                          , Order sector9 3
                          ]
                        )

        let subViews' = listSubViews "orders" view
        fst (selection (fieldInputChoice "product" (subViews' !! 1))) @=?
            "s9_ao"

    , testCase "Simple viewDisabled" $ do
        let view = runDatabase $ getForm "f" (ordersForm Nothing)
        True @=? viewDisabled "name" view
    ]


--------------------------------------------------------------------------------
testEnv :: Monad m => [(Text, Text)] -> FormEncType -> m (Env m)
testEnv input _formEncType = return $ \key -> return $ map (TextInput . snd) $
    filter ((== fromPath key) . fst) input


--------------------------------------------------------------------------------
selection :: [(Text, v, Bool)] -> (Text, v)
selection fic = head [(t, v) | (t, v, s) <- fic, s]

--------------------------------------------------------------------------------
selections :: [(Text, v, Bool)] -> [(Text, v)]
selections fic = [(t, v) | (t, v, s) <- fic, s]
