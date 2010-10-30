-- | Optionally failing transformers for forms
--
module Text.Digestive.Transformer 
    ( Transformer (..)
    , transform
    , transformEither
    , transformEitherM
    ) where

import Prelude hiding ((.), id)

import Control.Monad.Trans (lift)
import Control.Monad ((<=<))
import Control.Category (Category, (.), id)
import Control.Arrow (Arrow, arr, first)

import Text.Digestive.Result
import Text.Digestive.Types

newtype Transformer m e a b = Transformer
    { unTransformer :: a -> m (Either [e] b)
    }

instance Monad m => Category (Transformer m e) where
    id = Transformer $ return . Right
    f . g = Transformer $ \x -> unTransformer g x >>= \x' -> case x' of
        Left e -> return $ Left e
        Right y -> unTransformer f y

instance Monad m => Arrow (Transformer m e) where
    arr f = Transformer $ return . Right . f
    first t = Transformer $ \(x, y) -> unTransformer t x >>= \x' -> case x' of
        Left e -> return $ Left e
        Right x'' -> return $ Right (x'', y)

transform :: Monad m => Form m i e v a -> Transformer m e a b -> Form m i e v b
transform form transformer = Form $ do
    (v1, r1) <- unForm form
    range <- getFormRange
    case r1 of
        -- We already have an error, cannot continue
        Error e -> return (v1, Error e)
        -- Apply transformer
        Ok x -> do
            r2 <- lift $ lift $ unTransformer transformer x
            return $ case r2 of
                -- Attach the range information to the errors
                Left e -> (v1, Error $ map ((,) range) e)
                -- All fine
                Right y -> (v1, Ok y)

transformEither :: Monad m => (a -> Either e b) -> Transformer m e a b
transformEither f = transformEitherM $ return . f

transformEitherM :: Monad m => (a -> m (Either e b)) -> Transformer m e a b
transformEitherM f = Transformer $ 
    return . either (Left . return) (Right . id) <=< f
