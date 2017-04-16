--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
-- | Internal embedding of form fields with associated functions.
module Text.Digestive.Form.Internal.Field
    ( Field (..)
    , SomeField (..)
    , evalField
    , fieldMapView
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow        (second)
import           Data.Maybe           (listToMaybe, mapMaybe, catMaybes)
import           Data.Functor         ((<$>))
import           Data.List            (findIndex)
import           Data.Text            (Text)


--------------------------------------------------------------------------------
import           Text.Digestive.Types


--------------------------------------------------------------------------------
-- | A single input field. This usually maps to a single HTML @<input>@ element.
data Field v a where
    Singleton :: a -> Field v a
    Text      :: Text -> Field v Text
    -- A list of (group name, [(identifier, (value, view))]).
    -- Then we have the default index in the list.
    -- The return value has the actual value as well as the index in the list.
    Choice    :: [(Text, [(Text, (a, v))])] -> [Int] -> Field v [(a, Int)]
    Bool      :: Bool -> Field v Bool
    File      :: Field v [FilePath]


--------------------------------------------------------------------------------
instance Show (Field v a) where
    show (Singleton _) = "Singleton _"
    show (Text t)      = "Text " ++ show t
    show (Choice _ _)  = "Choice _ _"
    show (Bool b)      = "Bool " ++ show b
    show (File)        = "File"


--------------------------------------------------------------------------------
-- | Value agnostic "Field"
data SomeField v = forall a. SomeField (Field v a)


--------------------------------------------------------------------------------
-- | Evaluate a field to retrieve a value, using the given method and
-- a list of input.
evalField :: Method       -- ^ Get/Post
          -> [FormInput]  -- ^ Given input
          -> Field v a    -- ^ Field
          -> a            -- ^ Result
evalField _    _                 (Singleton x) = x
evalField _    (TextInput x : _) (Text _)      = x
evalField _    _                 (Text x)      = x
evalField _ ts@(TextInput _ : _) (Choice ls _) =
  let ls' = concat (map snd ls) in
    catMaybes $
      map (\(TextInput x) ->
        (\i -> (fst $ snd $ ls' !! i, i)) <$> findIndex (isSelectedChoice x . fst) ls') ts
evalField Get  _                 (Choice ls x) =
  -- this populates the default values when displaying a view
  let ls' = concat (map snd ls) in
    map (\i -> (fst $ snd $ ls' !! i, i)) x
evalField Post _                 (Choice _  _) = []
evalField Get  _                 (Bool x)      = x
evalField Post (TextInput x : _) (Bool _)      = x == "on"
evalField Post _                 (Bool _)      = False
evalField Post xs                File          = mapMaybe maybeFile xs
  where
    maybeFile (FileInput x) = Just x
    maybeFile _             = Nothing
evalField _    _                 File          = []


--------------------------------------------------------------------------------
-- | Map on the error message type of a Field.
fieldMapView :: (v -> w) -> Field v a -> Field w a
fieldMapView _ (Singleton x)   = Singleton x
fieldMapView _ (Text x)        = Text x
fieldMapView f (Choice xs i)   = Choice (map (second func) xs) i
  where func = map (second (second f))
fieldMapView _ (Bool x)        = Bool x
fieldMapView _ File            = File


--------------------------------------------------------------------------------
-- | Determines if the choiceVal is equal to the selectedVal.  Works with older
-- versions that submit "foo.bar.2" rather than "2" for Choice fields.
-- > isSelectedChoice "0" "0" == True
-- > isSelectedChoice "foo.bar.0" "0" == True
-- > isSelectedChoice "foo.bar.10" "0" == False
isSelectedChoice :: Text -> Text -> Bool
isSelectedChoice selectedVal choiceVal
  | selectedVal == choiceVal = True
  | otherwise =
    case listToMaybe (reverse $ toPath selectedVal) of
      Just x -> x == choiceVal
      _      -> False
