--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
module Text.Digestive.Form
    ( Formlet
    , Form
    , SomeForm (..)
    , (.:)

      -- * Basic forms
    , text
    , string
    , stringRead
    , choice
    , choice'
    , choiceWith
    , bool
    , file

      -- * Optional forms
    , optionalText
    , optionalString
    , optionalStringRead

      -- * Validation
    , check
    , checkM
    , validate
    , validateM

      -- * Lifting forms
    , monadic
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                (liftM)
import           Data.List                    (findIndex)
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as T


--------------------------------------------------------------------------------
import           Text.Digestive.Field
import           Text.Digestive.Form.Internal
import           Text.Digestive.Ref
import           Text.Digestive.Types
import           Text.Digestive.Util


--------------------------------------------------------------------------------
type Formlet m v a = Maybe a -> Form m v a


--------------------------------------------------------------------------------
text :: Formlet v m Text
text def = Pure Nothing $ Text $ fromMaybe "" def


--------------------------------------------------------------------------------
string :: Monad m => Formlet v m String
string = fmap T.unpack . text . fmap T.pack


--------------------------------------------------------------------------------
stringRead :: (Monad m, Read a, Show a) => v -> Formlet v m a
stringRead err = transform (readTransform err) . string . fmap show


--------------------------------------------------------------------------------
choice :: (Eq a, Monad m) => [(a, v)] -> Formlet v m a
choice items def = choice' items $
    maybe Nothing (\d -> findIndex ((== d) . fst) items) def


--------------------------------------------------------------------------------
-- | Sometimes there is no good 'Eq' instance for 'choice'. In this case, you
-- can use this function, which takes an index in the list as default.
choice' :: Monad m => [(a, v)] -> Maybe Int -> Form v m a
choice' items def = choiceWith (zip makeRefs items) def


--------------------------------------------------------------------------------
-- | Experimental
choiceWith :: Monad m => [(Text, (a, v))] -> Maybe Int -> Form v m a
choiceWith items def = fmap fst $ Pure Nothing $ Choice items def'
  where
    def' = fromMaybe 0 def


--------------------------------------------------------------------------------
bool :: Formlet v m Bool
bool = Pure Nothing . Bool . fromMaybe False


--------------------------------------------------------------------------------
file :: Form v m (Maybe FilePath)
file = Pure Nothing File


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
-- | Version of 'check' which allows monadic validations
checkM :: Monad m => v -> (a -> m Bool) -> Form v m a -> Form v m a
checkM err predicate form = validateM f form
  where
    f x = do
        r <- predicate x
        return $ if r then return x else Error err


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
-- | Version of 'validate' which allows monadic validations
validateM :: Monad m => (a -> m (Result v b)) -> Form v m a -> Form v m b
validateM = transform


--------------------------------------------------------------------------------
optionalText :: Monad m => Maybe Text -> Form v m (Maybe Text)
optionalText def = validate optional (text def)
  where
    optional t
        | T.null t  = return Nothing
        | otherwise = return $ Just t


--------------------------------------------------------------------------------
optionalString :: Monad m => Maybe String -> Form v m (Maybe String)
optionalString = fmap (fmap T.unpack) . optionalText . fmap T.pack


--------------------------------------------------------------------------------
optionalStringRead :: (Monad m, Read a, Show a)
                   => v -> Maybe a -> Form v m (Maybe a)
optionalStringRead err = transform readTransform' . optionalString . fmap show
  where
    readTransform' (Just s) = liftM (fmap Just) $ readTransform err s
    readTransform' Nothing  = return (return Nothing)


--------------------------------------------------------------------------------
readTransform :: (Monad m, Read a) => v -> String -> m (Result v a)
readTransform err = return . maybe (Error err) return . readMaybe
