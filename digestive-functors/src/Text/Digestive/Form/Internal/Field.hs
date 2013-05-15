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
import           Data.Maybe           (fromMaybe, listToMaybe)
import           Data.Text            (Text)


--------------------------------------------------------------------------------
import           Text.Digestive.Types


--------------------------------------------------------------------------------
-- | A single input field. This usually maps to a single HTML @<input>@ element.
data Field v a where
    Singleton :: a -> Field v a
    Text      :: Text -> Field v Text
    -- A list of identifier, value, view. Then we have the default index in
    -- the list. The return value has the actual value as well as the index in
    -- the list.
    Choice    :: [(Text, [(Text, (a, v))])] -> Int -> Field v (a, Int)
    Bool      :: Bool -> Field v Bool
    File      :: Field v (Maybe FilePath)


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
evalField _    (TextInput x : _) (Choice ls' y) =
  let ls = concat (map snd ls') in
    fromMaybe (fst (snd (ls !! y)), y) $ do
        -- Expects input in the form of "foo.bar.2". This is not needed for
        -- <select> fields, but we need it for labels for radio buttons.
        t      <- listToMaybe $ reverse $ toPath x
        (c, i) <- lookupIdx t ls
        return (fst c, i)
evalField _    _                 (Choice ls' x) =
  let ls = concat (map snd ls') in
    (fst (snd (ls !! x)), x)
evalField Get  _                 (Bool x)      = x
evalField Post (TextInput x : _) (Bool _)      = x == "on"
evalField Post _                 (Bool _)      = False
evalField Post (FileInput x : _) File          = Just x
evalField _    _                 File          = Nothing


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
-- | Retrieve the value and position of the value referenced to by the given
-- key in an association list, if the key is in the list.
lookupIdx :: Eq k => k -> [(k, v)] -> Maybe (v, Int)
lookupIdx key = go 0
  where
    go _  []        = Nothing
    go !i ((k, v) : xs)
        | key == k  = Just (v, i)
        | otherwise = go (i + 1) xs
