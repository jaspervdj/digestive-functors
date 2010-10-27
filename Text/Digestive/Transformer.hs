-- | Optionally failing transformers for forms
--
module Text.Digestive.Transformer where

import Text.Digestive.Types
import Control.Monad.Trans (lift)
import Control.Monad ((<=<))

newtype Transformer m e a b = Transformer
    { unTransformer :: a -> m (Either [e] b)
    }

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
