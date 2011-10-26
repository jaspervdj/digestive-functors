-- | Optionally failing transformers for forms
--
module Text.Digestive.Transform
    ( Transformer (..)
    , transform
    , transformFormlet
    , transformEither
    , transformEitherM
    , transformRead
    , required
    ) where

import Prelude hiding ((.), id)

import Control.Monad ((<=<))
import Control.Category (Category, (.), id)
import Control.Arrow (Arrow, arr, first)

import Text.Digestive.Result
import Text.Digestive.Types

-- | A transformer that transforms a value of type a to a value of type b
--
newtype Transformer m e a b = Transformer
    { unTransformer :: a -> m (Either [e] b)
    }

instance Monad m => Category (Transformer m e) where
    id = Transformer $ return . Right
    f . g = Transformer $   either (return . Left) (unTransformer f)
                        <=< unTransformer g

instance Monad m => Arrow (Transformer m e) where
    arr f = Transformer $ return . Right . f
    first t = Transformer $ \(x, y) -> unTransformer t x >>=
        return . either Left (Right . (flip (,) y))

-- | Apply a transformer to a form
--
transform :: Monad m => Form m i e v a -> Transformer m e a b -> Form m i e v b
transform form transformer = Form $ do
    (v, r) <- unForm form
    range <- getFormRange
    return (v, r >>= transform' range)
  where
    -- We already have an error, cannot continue
    transform' _     (Error e) = return (Error e)
    -- Apply transformer
    transform' range (Ok x)    = do
        ex <- unTransformer transformer x
        return $ case ex of
            -- Attach the range information to the errors
            Left e   -> Error $ map ((,) range) e
            -- All fine
            Right x' -> Ok x'

-- | Apply a transformer to a formlet
--
transformFormlet :: Monad m
                 => (b -> a)             -- ^ Needed to produce defaults
                 -> Formlet m i e v a    -- ^ Formlet to transform
                 -> Transformer m e a b  -- ^ Transformer
                 -> Formlet m i e v b    -- ^ Resulting formlet
transformFormlet f formlet transformer def =
    formlet (fmap f def) `transform` transformer

-- | Build a transformer from a simple function that returns an 'Either' result.
--
transformEither :: Monad m => (a -> Either e b) -> Transformer m e a b
transformEither f = transformEitherM $ return . f

-- | A monadic version of 'transformEither'
--
transformEitherM :: Monad m => (a -> m (Either e b)) -> Transformer m e a b
transformEitherM f = Transformer $ 
    return . either (Left . return) (Right . id) <=< f

-- | Create a transformer for any value of type a that is an instance of 'Read'
--
transformRead :: (Monad m, Read a)
              => e                         -- ^ Error given if read fails
              -> Transformer m e String a  -- ^ Resulting transformer
transformRead error' = transformEither $ \str -> case readsPrec 1 str of
    [(x, "")] -> Right x
    _ -> Left error'

-- | A transformer that converts 'Maybe a' to 'a'.
required :: (Monad m) => 
            e  -- ^ error to return if value is 'Nothing'
         -> Transformer m e (Maybe a) a
required err = transformEither $ maybe (Left err) Right
