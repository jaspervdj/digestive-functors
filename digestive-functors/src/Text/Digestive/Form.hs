--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE CPP                       #-}
-- | End-user interface - provides the main functionality for
-- form creation and validation. For an interface for front-end
-- implementation, see "View".
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
    , choiceWith'
    , choices
    , choices'
    , choicesWith
    , choicesWith'
    , groupedChoice
    , groupedChoice'
    , groupedChoiceWith
    , groupedChoiceWith'
    , bool
    , file

      -- * Optional forms
    , optionalText
    , optionalString
    , optionalStringRead

      -- * Date/time forms
    , utcTimeFormlet
    , localTimeFormlet
    , dateFormlet
    , timeFormlet
    , optionalUtcTimeFormlet
    , optionalLocalTimeFormlet
    , optionalDateFormlet
    , optionalTimeFormlet

      -- * Validation and transformation
    , check
    , checkM
    , validate
    , validateOptional
    , validateM
    , conditions
    , disable

      -- * Lifting forms
    , monadic

      -- * Dynamic list forms
    , listOf
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad                      (liftM, liftM2)
import           Data.List                          (findIndex)
import           Data.Maybe                         (fromMaybe, catMaybes)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
#if !MIN_VERSION_time(1,5,0)
import           System.Locale
#endif


--------------------------------------------------------------------------------
import           Text.Digestive.Form.Internal
import           Text.Digestive.Form.Internal.Field
import           Text.Digestive.Form.List
import           Text.Digestive.Ref
import           Text.Digestive.Types
import           Text.Digestive.Util


--------------------------------------------------------------------------------
-- | A 'Form' with a set, optional default value
type Formlet v m a = Maybe a -> Form v m a


--------------------------------------------------------------------------------
-- | Returns a 'Formlet' which may optionally take a default text
text :: Formlet v m Text
text def = Pure $ Text $ fromMaybe "" def


--------------------------------------------------------------------------------
-- | Identical to "text" but takes a String
string :: Monad m => Formlet v m String
string = fmap T.unpack . text . fmap T.pack


--------------------------------------------------------------------------------
-- | Returns a 'Formlet' for a parseable and serializable value type
stringRead :: (Monad m, Read a, Show a) => v -> Formlet v m a
stringRead err = transform (readTransform err) . string . fmap show


--------------------------------------------------------------------------------
-- | Returns a 'Formlet' for a value restricted to
-- the provided list of value-message tuples
choice :: (Eq a, Monad m) => [(a, v)] -> Formlet v m a
choice items def = choiceWith (zip makeRefs items) def


--------------------------------------------------------------------------------
-- | Sometimes there is no good 'Eq' instance for 'choice'. In this case, you
-- can use this function, which takes an index in the list as default.
choice' :: Monad m => [(a, v)] -> Maybe Int -> Form v m a
choice' items def = choiceWith' (zip makeRefs items) def


--------------------------------------------------------------------------------
-- | Allows you to assign your own values: these values will be used in the
-- resulting HTML instead of the default @[0 ..]@. This fixes some race
-- conditions that might otherwise appear, e.g. if new choice items are added to
-- some database while a user views and submits the form...
choiceWith :: (Eq a, Monad m) => [(Text, (a, v))] -> Formlet v m a
choiceWith items def = choiceWith' items def'
  where
    def' = def >>= (\d -> findIndex ((== d) . fst . snd) items)


--------------------------------------------------------------------------------
-- | A version of 'choiceWith' for when there is no good 'Eq' instance.
choiceWith' :: Monad m => [(Text, (a, v))] -> Maybe Int -> Form v m a
choiceWith' items def = fmap fst $ Pure $ Choice [("", items)] def'
  where
    def' = fromMaybe 0 def

-- | Returns a 'Formlet' for a values restricted to
-- the provided list of value-message tuples
choices :: (Eq a, Monad m) => [(a, v)] -> Formlet v m [a]
choices items def = choicesWith (zip makeRefs items) def

-- | Sometimes there is no good 'Eq' instance for 'choices'. In this case, you
-- can use this function, which takes an index in the list as default.
choices' :: Monad m => [(a, v)] -> Maybe [Int] -> Form v m [a]
choices' items def = choicesWith' (zip makeRefs items) def


--------------------------------------------------------------------------------
-- | Allows you to assign your own values: these values will be used in the
-- resulting HTML instead of the default @[0 ..]@. This fixes some race
-- conditions that might otherwise appear, e.g. if new choice items are added to
-- some database while a user views and submits the form...
choicesWith :: (Eq a, Monad m) => [(Text, (a, v))] -> Formlet v m [a]
choicesWith items def = choicesWith' items def'
  where
    def' = def >>= (\ds -> Just . catMaybes $ map (\d -> findIndex ((== d) . fst . snd) items) ds)

--------------------------------------------------------------------------------
-- | A version of 'choicesWith' for when there is no good 'Eq' instance.
choicesWith' :: Monad m => [(Text, (a, v))] -> Maybe [Int] -> Form v m [a]
choicesWith' items def = fmap (\v -> map fst v) $ Pure $ Choices [([""], items)] def'
  where
    def' = fromMaybe [] def

--------------------------------------------------------------------------------
-- | Returns a 'Formlet' for named groups of choices.
groupedChoice :: (Eq a, Monad m) => [(Text, [(a, v)])] -> Formlet v m a
groupedChoice items def =
    groupedChoiceWith (mkGroupedRefs items makeRefs) def


--------------------------------------------------------------------------------
-- | Sometimes there is no good 'Eq' instance for 'choice'. In this case, you
-- can use this function, which takes an index in the list as default.
groupedChoice' :: Monad m => [(Text, [(a, v)])] -> Maybe Int -> Form v m a
groupedChoice' items def =
    groupedChoiceWith' (mkGroupedRefs items makeRefs) def


mkGroupedRefs :: [(Text, [a])]
              -> [Text]
              -> [(Text, [(Text, a)])]
mkGroupedRefs [] _ = []
mkGroupedRefs (g:gs) is = cur : mkGroupedRefs gs b
  where
    (a,b) = splitAt (length $ snd g) is
    cur = (fst g, zip a (snd g))


--------------------------------------------------------------------------------
-- | Allows you to assign your own values: these values will be used in the
-- resulting HTML instead of the default @[0 ..]@. This fixes some race
-- conditions that might otherwise appear, e.g. if new choice items are added to
-- some database while a user views and submits the form...
groupedChoiceWith :: (Eq a, Monad m)
                  => [(Text, [(Text, (a, v))])]
                  -> Formlet v m a
groupedChoiceWith items def = groupedChoiceWith' items def'
  where
    def' = def >>= (\d -> findIndex ((== d) . fst . snd) $
                            concat $ map snd items)


--------------------------------------------------------------------------------
-- | Low-level support for grouped choice.
groupedChoiceWith' :: Monad m
                   => [(Text, [(Text, (a, v))])]
                   -> Maybe Int
                   -> Form v m a
groupedChoiceWith' items def = fmap fst $ Pure $ Choice items def'
  where
    def' = fromMaybe 0 def


--------------------------------------------------------------------------------
-- | Returns a 'Formlet' for binary choices
bool :: Formlet v m Bool
bool = Pure . Bool . fromMaybe False


--------------------------------------------------------------------------------
-- | Returns a 'Formlet' for file selection
file :: Form v m (Maybe FilePath)
file = Pure File


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
validate :: Monad m => (a -> Result v b) -> Form v m a -> Form v m b
validate = validateM . (return .)

--------------------------------------------------------------------------------
-- | Same as 'validate', but works with forms of the form:
--
-- >  Form v m (Maybe a)
--
-- .
--
-- Example: taking the first character of an optional input string
--
-- > head' :: String -> Result String Char
-- > head' []      = Error "Is empty"
-- > head' (x : _) = Success x
-- >
-- > char :: Monad m => Form m String (Maybe Char)
-- > char = validateOptional head' (optionalString Nothing)
validateOptional :: Monad m => (a -> Result v b) -> Form v m (Maybe a) -> Form v m (Maybe b)
validateOptional f = validate (forOptional f)

--------------------------------------------------------------------------------
-- | Version of 'validate' which allows monadic validations
validateM :: Monad m => (a -> m (Result v b)) -> Form v m a -> Form v m b
validateM = transform

--------------------------------------------------------------------------------
-- | Allows for the composition of independent validation functions.
--
-- For example, let's validate an even integer between 0 and 100:
--
-- > form :: Monad m => Form Text m FormData
-- > ... -- some fields
-- > <*> "smallEvenInteger" .: validate (notEmpty >=> integer >=> even >=> greaterThan 0 >=> lessThanOrEq 100) (text Nothing)
-- > ... -- more fields
--
-- where
--
-- > notEmpty       :: IsString v => Text -> Result v Text
-- > integer        :: (Integral a, IsString v) => Text -> Result v a
-- > greaterThan  0 :: (Num a, Ord a, Show a) => a -> Result Text a
-- > lessThanOrEq 0 :: (Num a, Ord a, Show a) => a -> Result Text a
-- > even           :: Integer -> Result Text Integer
--
-- .
--
-- This will validate our smallEvenInteger correctly, but there is a problem.
-- If a user enters an odd number greater than 100, only
--
-- > "number must be even"
--
--
-- will be returned. It would make for a better user experience if
--
-- > ["number must be even", "number must be less than 100"]
--
-- was returned instead. This can be accomplished by rewriting our form to be:
--
-- > form :: Monad m => Form [Text] m FormData
-- > ... -- some fields
-- > <*> "smallEvenInteger" .: validate (notEmpty >=> integer >=> conditions [even, greaterThan 0, lessThanOrEq 100]) (text Nothing)
-- > ... -- more fields
--
-- .
--
-- If we want to collapse our list of errors into a single 'Text', we can do something like:
--
-- > form :: Monad m => Form Text m FormData
-- > ... -- some fields
-- > <*> "smallEvenInteger" .: validate (notEmpty >=> integer >=> commaSeperated . conditions [even, greaterThan 0, lessThanOrEq 100]) (text Nothing)
-- > ... -- more fields
--
-- where
--
-- > commaSeperated :: (Result [Text] a) -> (Result Text a)
--
-- .
conditions :: [(a -> Result e b)] -- ^ Any 'Success' result of  a validation function is provably guaranteed to be discarded. Only 'Error' results are used.
           -> a -- ^ If all validation functions pass, parameter will be re-wrapped with a 'Success'.
           -> (Result [e] a) -- ^ List of errors is guaranteed to be in the same order as inputed validations functions. So,
                             --
                             -- > conditions [even,  greaterThan 0] -1
                             --
                             -- is specified to return
                             --
                             -- > Error ["must be even", "must be greater than 0"]
                             --
                             -- and not
                             --
                             -- > Error ["must be greater than 0", "must be even"]
                             --
                             -- .
conditions fs x = result
  where
    result = case (foldr errorCond [] fs) of
      [] -> Success x
      es -> Error es
    errorCond f es = case (f x) of
      Success _ -> es
      Error   e -> e:es


--------------------------------------------------------------------------------
-- | Disables a form
disable :: Form v m a -> Form v m a
disable f = Metadata [Disabled] f


--------------------------------------------------------------------------------
-- | Create a text form with an optional default text which
-- returns nothing if no optional text was set, and no input
-- was retrieved.
optionalText :: Monad m => Maybe Text -> Form v m (Maybe Text)
optionalText def = validate opt (text def)
  where
    opt t
        | T.null t  = return Nothing
        | otherwise = return $ Just t


--------------------------------------------------------------------------------
-- | Identical to 'optionalText', but uses Strings
optionalString :: Monad m => Maybe String -> Form v m (Maybe String)
optionalString = fmap (fmap T.unpack) . optionalText . fmap T.pack


--------------------------------------------------------------------------------
-- | Identical to 'optionalText' for parseable and serializable values.
optionalStringRead :: (Monad m, Read a, Show a)
                   => v -> Maybe a -> Form v m (Maybe a)
optionalStringRead err = transform readTransform' . optionalString . fmap show
  where
    readTransform' (Just s) = liftM (fmap Just) $ readTransform err s
    readTransform' Nothing  = return (return Nothing)


--------------------------------------------------------------------------------
-- Helper function for attempted parsing, with custom error messages
readTransform :: (Monad m, Read a) => v -> String -> m (Result v a)
readTransform err = return . maybe (Error err) return . readMaybe


--------------------------------------------------------------------------------
-- | Dynamic lists
listOf :: Monad m
       => Formlet v m a
       -> Formlet v m [a]
listOf single def =
    List (fmap single defList) (indicesRef .: listIndices ixs)
  where
    ixs = case def of
        Nothing -> [0]
        Just xs -> [0 .. length xs - 1]

    defList = DefaultList Nothing $ maybe [] (map Just) def


--------------------------------------------------------------------------------
-- Manipulatable indices
listIndices :: Monad m => [Int] -> Form v m [Int]
listIndices = fmap parseIndices . text . Just . unparseIndices


------------------------------------------------------------------------------
--                            Date/time formlets
------------------------------------------------------------------------------


utcTimeFormlet :: Monad m
               => String
                 -- ^ Date format string
               -> String
                 -- ^ Time format string
               -> TimeZone
               -> Formlet Text m UTCTime
utcTimeFormlet dFmt tFmt tz d =
    localTimeToUTC tz <$> localTimeFormlet dFmt tFmt (utcToLocalTime tz <$> d)


localTimeFormlet :: Monad m
                 => String
                   -- ^ Date format string
                 -> String
                   -- ^ Time format string
                 -> Formlet Text m LocalTime
localTimeFormlet dFmt tFmt d = LocalTime
      <$> "date" .: dateFormlet dFmt (localDay <$> d)
      <*> "time" .: timeFormlet tFmt (localTimeOfDay <$> d)


dateFormlet :: Monad m
            => String
              -- ^ Format string
            -> Formlet Text m Day
dateFormlet fmt d =
    validate (vFunc fmt "invalid date") (string $ formatTime defaultTimeLocale fmt <$> d)


timeFormlet :: Monad m
            => String
              -- ^ Format string
            -> Formlet Text m TimeOfDay
timeFormlet fmt d =
    validate (vFunc fmt "invalid time") (string $ formatTime defaultTimeLocale fmt <$> d)


optionalUtcTimeFormlet :: Monad m
                       => String
                         -- ^ Date format string
                       -> String
                         -- ^ Time format string
                       -> TimeZone
                       -> Maybe UTCTime
                       -> Form Text m (Maybe UTCTime)
optionalUtcTimeFormlet dFmt tFmt tz d =
    liftM (localTimeToUTC tz) <$> optionalLocalTimeFormlet dFmt tFmt (utcToLocalTime tz <$> d)


optionalLocalTimeFormlet :: Monad m
                         => String
                           -- ^ Date format string
                         -> String
                           -- ^ Time format string
                         -> Maybe LocalTime
                         -> Form Text m (Maybe LocalTime)
optionalLocalTimeFormlet dFmt tFmt d = liftM2 LocalTime
      <$> "date" .: optionalDateFormlet dFmt (localDay <$> d)
      <*> "time" .: optionalTimeFormlet tFmt (localTimeOfDay <$> d)


optionalDateFormlet :: Monad m
                    => String
                      -- ^ Format string
                    -> Maybe Day
                    -> Form Text m (Maybe Day)
optionalDateFormlet fmt d =
    validate (vOpt $ vFunc fmt "invalid date") (string $ formatTime defaultTimeLocale fmt <$> d)


optionalTimeFormlet :: Monad m
                    => String
                      -- ^ Format string
                    -> Maybe TimeOfDay
                    -> Form Text m (Maybe TimeOfDay)
optionalTimeFormlet fmt d =
    validate (vOpt $ vFunc fmt "invalid time") (string $ formatTime defaultTimeLocale fmt <$> d)


vFunc :: ParseTime a => String -> Text -> String -> Result Text a
vFunc fmt err x
#if MIN_VERSION_time(1,5,0)
  | length x < 40 = maybe (Error err) Success $ parseTimeM True defaultTimeLocale fmt x
#else
  | length x < 40 = maybe (Error err) Success $ parseTime defaultTimeLocale fmt x
#endif
  | otherwise = Error "Not a valid date/time string"


vOpt :: (String -> Result v a) -> String -> Result v (Maybe a)
vOpt _ "" = Success Nothing
vOpt f x  = Just <$> f x
