{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings, Rank2Types #-}
module Text.Digestive.Form
    ( Formlet
    , Form
    , SomeForm (..)
    , (.:)

      -- * Forms
    , text
    , string
    , stringRead
    , choice
    , bool
    , file

      -- * Validation
    , check
    , checkM
    , validate
    , validateM
    ) where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Digestive.Field
import Text.Digestive.Form.Internal
import Text.Digestive.Types
import Text.Digestive.Util

type Formlet m v a = Maybe a -> Form m v a

text :: Formlet v m Text
text def = Pure Nothing $ Text $ fromMaybe "" def

string :: Monad m => Formlet v m String
string = fmap T.unpack . text . fmap T.pack

stringRead :: (Monad m, Read a, Show a) => v -> Formlet v m a
stringRead err = transform readTransform . string . fmap show
  where
    readTransform = return . maybe (Error err) return . readMaybe

choice :: Eq a => [(a, v)] -> Formlet v m a
choice items def = Pure Nothing $ Choice items $ fromMaybe 0 $
    maybe Nothing (\d -> findIndex ((== d) . fst) items) def

bool :: Formlet v m Bool
bool = Pure Nothing . Bool . fromMaybe False

file :: Form v m (Maybe FilePath)
file = Pure Nothing File

-- | Validate the results of a form with a simple predicate
--
-- Example:
--
-- > check "Can't be empty" (not . null) (string Nothing)
check :: Monad m
      => v            -- ^ Error message (if fail)
      -> (a -> Bool)  -- ^ Validating predicate
      -> Form v m a   -- ^ Form to validate
      -> Form v m a   -- ^ Resulting form
check err = checkM err . (return .)

-- | Version of 'check' which allows monadic validations
checkM :: Monad m => v -> (a -> m Bool) -> Form v m a -> Form v m a
checkM err predicate form = validateM f form
  where
    f x = do
        r <- predicate x
        return $ if r then return x else Error err

-- | This is an extension of 'check' that can be used to apply transformations
-- that optionally fail
--
-- Example: taking the first character of an input string
--
-- > head' :: String -> Result String Char
-- > head' []      = Error "Is empty"
-- > head' (x : _) = Success x
-- >
-- > char :: Monad m => Form m String Char
-- > char = validate head' (string Nothing)
--
validate :: Monad m => (a -> Result v b) -> Form v m a -> Form v m b
validate = validateM . (return .)

-- | Version of 'validate' which allows monadic validations
validateM :: Monad m => (a -> m (Result v b)) -> Form v m a -> Form v m b
validateM = transform
