{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings, Rank2Types #-}
module Text.Digestive.Form
    ( Form
    , SomeForm (..)
    , (.:)

      -- * Forms
    , text
    , string
    , stringRead
    , choice
    , bool

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

text :: Maybe Text -> Form m v Text
text def = Pure Nothing $ Text $ fromMaybe "" def

string :: Monad m => Maybe String -> Form m v String
string = fmap T.unpack . text . fmap T.pack

stringRead :: (Monad m, Read a, Show a) => v -> Maybe a -> Form m v a
stringRead err = transform readTransform . string . fmap show
  where
    readTransform = return . maybe (Error err) return . readMaybe

choice :: Eq a => [(a, v)] -> Maybe a -> Form m v a
choice items def = Pure Nothing $ Choice items $ fromMaybe 0 $
    maybe Nothing (\d -> findIndex ((== d) . fst) items) def

bool :: Bool -> Form m v Bool
bool = Pure Nothing . Bool

check :: Monad m => v -> (a -> Bool) -> Form m v a -> Form m v a
check err = checkM err . (return .)

checkM :: Monad m => v -> (a -> m Bool) -> Form m v a -> Form m v a
checkM err predicate form = validateM f form
  where
    f x = do
        r <- predicate x
        return $ if r then return x else Error err

validate :: Monad m => (a -> Result v b) -> Form m v a -> Form m v b
validate = validateM . (return .)

validateM :: Monad m => (a -> m (Result v b)) -> Form m v a -> Form m v b
validateM = transform
