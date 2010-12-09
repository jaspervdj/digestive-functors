{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Text.Digestive.Forms.Tests
    ( tests
    ) where

import Control.Applicative ((<$>), (<*>))

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@?), (@=?))

import Text.Digestive.Tests.Util
import Text.Digestive.Result
import Text.Digestive.Types
import Text.Digestive.Common
import Text.Digestive.Forms

tests :: [Test]
tests = [ testProperty "pass through"    passThrough
        , testProperty "compose"         compose
        , testCase     "label ID"        labelId
        , testCase     "test input bool" testInputBool
        ]

instance FormInput String () where
    getInputString = Just
    getInputFile = const Nothing

-- Build a test case: give a string as only input, run it through a form, the
-- result should stay the same
passThrough :: String -> Bool
passThrough inp = unId $ do
    -- Find out an ID
    id' <- viewForm form "form"
    er <- eitherForm form "form" (environment id')
    return $ er == Right inp
 where
    -- Our simple form
    form = inputString (\x _ -> x) Nothing
    environment id' = fromList [(id', inp)]

-- A form composing two values
compose :: Int -> String -> Bool
compose a b = unId $ do
    [aId, bId] <- viewForm form "form"
    er <- eitherForm form "form" $ environment aId bId
    return $ er == Right (a, b)
  where
    view' x _ = [x]
    form = (,) <$> inputRead view' "read error" Nothing
               <*> inputString view' Nothing
    environment aId bId = fromList [(aId, show a), (bId, b)]

-- Check that the label ID stays the same
labelId :: Assertion
labelId = unId $ do
    [l1, l2, l3] <- viewForm form "form"
    return $ l1 == l2 && l2 == l3 @? "ID's should be the same"
  where
    form :: Form Id String e [FormId] String
    form = label return ++> inputString (\x _ -> [x]) Nothing <++ label return

-- Check that bool inputs work
testInputBool :: Assertion
testInputBool = unId $ do
    [l1, l2, l3] <- viewForm form "form"
    er <- eitherForm form "form" $ environment l1 l2 l3
    return $ er @=? Right (False, True, False)
  where
    view' x _ = [x]
    form = (,,) <$> inputBool view' True
                <*> inputBool view' True
                <*> inputBool view' True
    environment l1 l2 _ = fromList [(l1, ""), (l2, "on")]
