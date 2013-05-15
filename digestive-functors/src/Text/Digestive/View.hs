--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- | Provides functionality for frontend and backend integration.
--
-- This module contains functions used to glue form handling to
-- particular server implementations and view libraries, defining
-- the standard behaviour for handling GET and POST requests.
--
-- Field accessors can be used to write frontend libraries,
-- mapping field values to concrete elements.
module Text.Digestive.View
    ( View (..)

      -- * Obtaining a view
    , getForm
    , postForm

      -- * Operations on views
    , subView
    , subViews

      -- * Querying a view
      -- ** Low-level
    , absolutePath
    , absoluteRef

      -- ** Form encoding
    , viewEncType

      -- ** Input
    , fieldInputText
    , fieldInputChoice
    , fieldInputChoiceGroup
    , fieldInputBool
    , fieldInputFile

      -- ** List subview
    , listSubViews
    , makeListSubView

      -- ** Errors
    , errors
    , childErrors

      -- * Debugging
    , debugViewPaths
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                      (second)
import           Control.Monad.Identity             (Identity)
import           Data.List                          (isPrefixOf)
import           Data.Text                          (Text)
import qualified Data.Text                          as T


--------------------------------------------------------------------------------
import           Text.Digestive.Form.Encoding
import           Text.Digestive.Form.Internal
import           Text.Digestive.Form.Internal.Field
import           Text.Digestive.Form.List
import           Text.Digestive.Types


--------------------------------------------------------------------------------
-- | Finalized form - handles the form, error messages and input.
-- Internally handles the addressing of individual fields.
data View v = forall a m. Monad m => View
    { viewName    :: Text
    , viewContext :: Path
    , viewForm    :: FormTree Identity v m a
    , viewInput   :: [(Path, FormInput)]
    , viewErrors  :: [(Path, v)]
    , viewMethod  :: Method
    }


--------------------------------------------------------------------------------
instance Functor View where
    fmap f (View name ctx form input errs method) = View
        name ctx (formMapView f form) input (map (second f) errs) method


--------------------------------------------------------------------------------
instance Show v => Show (View v) where
    show (View name ctx form input errs method) =
        "View " ++ show name ++ " " ++ show ctx ++ " " ++ show form ++ " " ++
        show input ++ " " ++ show errs ++ " " ++ show method


--------------------------------------------------------------------------------
-- | Serve up a form for a GET request - no form input
getForm :: Monad m => Text -> Form v m a -> m (View v)
getForm name form = do
    form' <- toFormTree form
    return $ View name [] form' [] [] Get


--------------------------------------------------------------------------------
-- | Handle a form for a POST request - evaluate with the given environment
-- and return the result.
postForm :: Monad m => Text -> Form v m a -> Env m -> m (View v, Maybe a)
postForm name form env = do
    form' <- toFormTree form
    eval Post env' form' >>= \(r, inp) -> return $ case r of
        Error errs -> (View name [] form' inp errs Post, Nothing)
        Success x  -> (View name [] form' inp [] Post, Just x)
  where
    env' = env . (name :)


--------------------------------------------------------------------------------
-- | Returns the subview of a view matching the given serialized 'Path'
subView :: Text -> View v -> View v
subView ref (View name ctx form input errs method) =
    case lookupForm path form of
        []               ->
            View name (ctx ++ path) notFound (strip input) (strip errs) method
        (SomeForm f : _) ->
            View name (ctx ++ path) f (strip input) (strip errs) method
  where
    path     = toPath ref
    lpath    = length path

    strip :: [(Path, a)] -> [(Path, a)]
    strip xs = [(drop lpath p, x) | (p, x) <- xs, path `isPrefixOf` p]

    notFound :: FormTree Identity v Identity a
    notFound = error $ "Text.Digestive.View.subView: " ++
        "No such subView: " ++ T.unpack ref


--------------------------------------------------------------------------------
-- | Returns all immediate subviews of a view
subViews :: View v -> [View v]
subViews view@(View _ _ form _ _ _) =
    [subView r view | r <- go (SomeForm form)]
  where
    go (SomeForm f) = case getRef f of
        Nothing -> [r | c <- children f, r <- go c]
        Just r  -> [r]


--------------------------------------------------------------------------------
-- | Determine an absolute 'Path' for a field in the form
absolutePath :: Text -> View v -> Path
absolutePath ref (View name ctx _ _ _ _) = name : (ctx ++ toPath ref)


--------------------------------------------------------------------------------
-- | Determine an absolute path and call 'fromPath' on it. Useful if you're
-- writing a view library...
absoluteRef :: Text -> View v -> Text
absoluteRef ref view = fromPath $ absolutePath ref view


--------------------------------------------------------------------------------
-- | Returns the content type of the View - depends on contained fields
viewEncType :: View v -> FormEncType
viewEncType (View _ _ form _ _ _) = formTreeEncType form


--------------------------------------------------------------------------------
-- Return form inputs which are paired with a path identical to the argument
lookupInput :: Path -> [(Path, FormInput)] -> [FormInput]
lookupInput path = map snd . filter ((== path) . fst)


--------------------------------------------------------------------------------
-- | Return the text data at the position referred to by the given
-- serialized Path.
fieldInputText :: forall v. Text -> View v -> Text
fieldInputText ref (View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = toPath ref
    givenInput = lookupInput path input

    eval' :: Field v b -> Text
    eval' field = case field of
        Text t -> evalField method givenInput (Text t)
        f      -> error $ T.unpack ref ++ ": expected (Text _), " ++
            "but got: (" ++ show f ++ ")"


--------------------------------------------------------------------------------
-- | Returns a list of (identifier, view, selected?)
fieldInputChoice :: forall v. Text -> View v -> [(Text, v, Bool)]
fieldInputChoice ref (View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = toPath ref
    givenInput = lookupInput path input

    eval' :: Field v b -> [(Text, v, Bool)]
    eval' field = case field of
        Choice xs didx ->
            let idx = snd $ evalField method givenInput (Choice xs didx)
            in map (\(i, (k, (_, v))) -> (k, v, i == idx)) $
                 zip [0 ..] $ concat $ map snd xs
        f           -> error $ T.unpack ref ++ ": expected (Choice _ _), " ++
            "but got: (" ++ show f ++ ")"


--------------------------------------------------------------------------------
-- | Returns a list of (groupName, [(identifier, view, selected?)])
fieldInputChoiceGroup :: forall v. Text
                      -> View v
                      -> [(Text, [(Text, v, Bool)])]
fieldInputChoiceGroup ref (View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = toPath ref
    givenInput = lookupInput path input

    eval' :: Field v b -> [(Text, [(Text, v, Bool)])]
    eval' field = case field of
        Choice xs didx ->
            let idx = snd $ evalField method givenInput (Choice xs didx)
            in merge idx xs [0..]
        f           -> error $ T.unpack ref ++ ": expected (Choice _ _), " ++
            "but got: (" ++ show f ++ ")"

merge :: Int
      -> [(Text, [(Text, (a, v))])]
      -> [Int]
      -> [(Text, [(Text, v, Bool)])]
merge _ [] _ = []
merge idx (g:gs) is = cur : merge idx gs b
  where
    (a,b) = splitAt (length $ snd g) is
    cur = (fst g, map (\(i, (k, (_, v))) -> (k, v, i == idx)) $ zip a (snd g))

--------------------------------------------------------------------------------
-- | Returns True/False based on the field referred to by the given
-- serialized Path.
fieldInputBool :: forall v. Text -> View v -> Bool
fieldInputBool ref (View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = toPath ref
    givenInput = lookupInput path input

    eval' :: Field v b -> Bool
    eval' field = case field of
        Bool x -> evalField method givenInput (Bool x)
        f      -> error $ T.unpack ref ++ ": expected (Bool _), " ++
            "but got: (" ++ show f ++ ")"


--------------------------------------------------------------------------------
-- | Return the FilePath referred to by the given serialized path, if set.
fieldInputFile :: forall v. Text -> View v -> Maybe FilePath
fieldInputFile ref (View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = toPath ref
    givenInput = lookupInput path input

    eval' :: Field v b -> Maybe FilePath
    eval' field = case field of
        File -> evalField method givenInput File
        f    -> error $ T.unpack ref ++ ": expected (File), " ++
            "but got: (" ++ show f ++ ")"


--------------------------------------------------------------------------------
-- | Returns sub views referred to by dynamic list indices
-- at the given serialized path.
listSubViews :: Text -> View v -> [View v]
listSubViews ref view =
    map (\i -> makeListSubView ref i view) indices
  where
    path        = toPath ref
    indicesPath = path ++ toPath indicesRef
    indices     = parseIndices $ fieldInputText (fromPath indicesPath) view


--------------------------------------------------------------------------------
-- | Creates a sub view
makeListSubView :: Text
                -- ^ ref
                -> Int
                -- ^ index to use for the subview
                -> View v
                -- ^ list view
                -> View v
makeListSubView ref ind view@(View _ _ form _ _ _) =
    case subView (fromPath $ path ++ [T.pack $ show ind]) view of
        View name ctx _ input errs method ->
            case lookupList path form of
                -- TODO don't use head
                (SomeForm (List defs _)) ->
                    View name ctx (defs `defaultListIndex` ind)
                        input errs method
                _                                -> error $
                    T.unpack ref ++ ": expected List, but got another form"
  where
    path        = toPath ref


--------------------------------------------------------------------------------
-- | Returns all errors related to the form corresponding to the given
-- serialized Path
errors :: Text -> View v -> [v]
errors ref = map snd . filter ((== toPath ref) . fst) . viewErrors


--------------------------------------------------------------------------------
-- | Returns all errors related to the form, and its children, pointed
-- to by the given serialized Path.
childErrors :: Text -> View v -> [v]
childErrors ref = map snd .
    filter ((toPath ref `isPrefixOf`) . fst) . viewErrors


--------------------------------------------------------------------------------
-- | Retrieve all paths of the contained form
debugViewPaths :: View v -> [Path]
debugViewPaths (View _ _ form _ _ _) = debugFormPaths form
