{-# LANGUAGE ExistentialQuantification, GADTs, OverloadedStrings,
        ScopedTypeVariables #-}
module Text.Digestive.View
    ( View (..)

      -- * Obtaining a view
    , getForm
    , postForm

      -- * Operations on views
    , subView

      -- * Querying a view
      -- ** Low-level
    , absolutePath
    , absoluteRef

      -- ** Form encoding
    , viewEncType

      -- ** Input
    , fieldInputText
    , fieldInputChoice
    , fieldInputBool
    , fieldInputFile

      -- ** Errors
    , errors
    , childErrors

      -- ** Scaffolding
    , scaffold
    ) where

import Control.Arrow (second)
import Data.List (findIndex, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mappend)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Digestive.Field
import Text.Digestive.Form.Encoding
import Text.Digestive.Form.Internal
import Text.Digestive.Types

data View v = forall a m. Monad m => View
    { viewName    :: Text
    , viewContext :: Path
    , viewForm    :: Form v m a
    , viewInput   :: [(Path, FormInput)]
    , viewErrors  :: [(Path, v)]
    , viewMethod  :: Method
    }

instance Functor View where
    fmap f (View name ctx form input errs method) = View
        name ctx (formMapView f form) input (map (second f) errs) method

instance Show v => Show (View v) where
    show (View name ctx form input errs method) =
        "View " ++ show name ++ " " ++ show ctx ++ " " ++ show form ++ " " ++
        show input ++ " " ++ show errs ++ " " ++ show method

getForm :: Monad m => Text -> Form v m a -> View v
getForm name form = View name [] form [] [] Get

postForm :: Monad m => Text -> Form v m a -> Env m -> m (View v, Maybe a)
postForm name form env = eval Post env' form >>= \(r, inp) -> return $ case r of
    Error errs -> (View name [] form inp errs Post, Nothing)
    Success x  -> (View name [] form inp [] Post, Just x)
  where
    env' = env . (name :)

subView :: Text -> View v -> View v
subView ref (View name ctx form input errs method) =
    View name (ctx ++ path) form input errs method
  where
    path = toPath ref

-- | Determine an absolute 'Path' for a field in the form
absolutePath :: Text -> View v -> Path
absolutePath ref view@(View name _ _ _ _ _) = name : viewPath ref view

-- | Determine an absolute path and call 'fromPath' on it. Useful if you're
-- writing a view library...
absoluteRef :: Text -> View v -> Text
absoluteRef ref view = fromPath $ absolutePath ref view

-- | Internal version of 'absolutePath' which does not take the form name into
-- account
viewPath :: Text -> View v -> Path
viewPath ref (View _ ctx _ _ _ _) = ctx ++ toPath ref

viewEncType :: View v -> FormEncType
viewEncType (View _ _ form _ _ _) = formEncType form

lookupInput :: Path -> [(Path, FormInput)] -> [FormInput]
lookupInput path = map snd . filter ((== path) . fst)

fieldInputText :: forall v. Text -> View v -> Text
fieldInputText ref view@(View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = viewPath ref view
    givenInput = lookupInput path input

    eval' :: Field v b -> Text
    eval' field = case field of
        Text t -> evalField method givenInput (Text t)
        f      -> error $ T.unpack ref ++ ": expected (Text _), " ++
            "but got: (" ++ show f ++ ")"

fieldInputChoice :: forall v. Text -> View v -> ([v], Int)
fieldInputChoice ref view@(View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = viewPath ref view
    givenInput = lookupInput path input

    eval' :: Field v b -> ([v], Int)
    eval' field = case field of
        Choice xs i ->
            let x   = evalField method givenInput (Choice xs i)
                idx = fromMaybe 0 $ findIndex (== x) (map fst xs)
            in (map snd xs, idx)
        f           -> error $ T.unpack ref ++ ": expected (Choice _ _), " ++
            "but got: (" ++ show f ++ ")"

fieldInputBool :: forall v. Text -> View v -> Bool
fieldInputBool ref view@(View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = viewPath ref view
    givenInput = lookupInput path input

    eval' :: Field v b -> Bool
    eval' field = case field of
        Bool x -> evalField method givenInput (Bool x)
        f      -> error $ T.unpack ref ++ ": expected (Bool _), " ++
            "but got: (" ++ show f ++ ")"

fieldInputFile :: forall v. Text -> View v -> Maybe FilePath
fieldInputFile ref view@(View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = viewPath ref view
    givenInput = lookupInput path input

    eval' :: Field v b -> Maybe FilePath
    eval' field = case field of
        File -> evalField method givenInput File
        f    -> error $ T.unpack ref ++ ": expected (File), " ++
            "but got: (" ++ show f ++ ")"

errors :: Text -> View v -> [v]
errors ref view = map snd $ filter ((== viewPath ref view) . fst) $
    viewErrors view

childErrors :: Text -> View v -> [v]
childErrors ref view = map snd $
    filter ((viewPath ref view `isPrefixOf`) . fst) $ viewErrors view

scaffold :: Monoid a
         => (Path -> a)  -- ^ Label
         -> a            -- ^ Break after input field
         -> (Path -> a)  -- ^ Singleton fields
         -> (Path -> a)  -- ^ Text fields
         -> (Path -> a)  -- ^ Choice fields
         -> (Path -> a)  -- ^ Bool fields
         -> (Path -> a)  -- ^ File fields
         -> View v       -- ^ View to fold over
         -> a            -- ^ Result
scaffold label br singleton text choice bool file (View _ _ form _ _ _) =
    foldForm
        (\p -> label p `mappend` singleton p `mappend` br)
        (\p -> label p `mappend` text p      `mappend` br)
        (\p -> label p `mappend` choice p    `mappend` br)
        (\p -> label p `mappend` bool p      `mappend` br)
        (\p -> label p `mappend` file p      `mappend` br)
        form
