-- | Validators that can be attached to forms
--
module Text.Digestive.Validator where

import Control.Monad (liftM2)
import Control.Monad.Trans (lift)
import Data.Monoid (Monoid (..))

import Text.Digestive.Types

-- | A validator
--
newtype Validator m a = Validator {unValidator :: a -> m [String]}

instance Monad m => Monoid (Validator m inp) where
    mempty = Validator $ const $ return mempty
    v1 `mappend` v2 = Validator $ \inp ->
        liftM2 mappend (unValidator v1 inp) (unValidator v2 inp)

-- | Attach 'Validator's to a 'Form'
--
validate :: Monad m => Form m inp v a -> [Validator m a] -> Form m inp v a
validate form validators = Form $ do
    (view', result) <- unForm form
    range <- getFormRange
    -- Check the result
    case result of
        -- We already have an error, no more validation needed
        Error e -> return (view', Error e)
        -- Allright, now apply the validator
        Ok x -> do
            r <- lift $ lift $ unValidator (mconcat validators) x
            return $ case r of
                [] -> (view', Ok x)
                -- Attach the range information to the errors
                e -> (view', Error $ map ((,) range) e)

-- | Easy way to create a pure validator
--
check :: Monad m
      => String         -- ^ Error message
      -> (a -> Bool)    -- ^ Actual validation
      -> Validator m a  -- ^ Resulting validator
check error' = checkM error' . (return .)

-- | Easy way to create a monadic validator
--
checkM :: Monad m
       => String         -- ^ Error message
       -> (a -> m Bool)  -- ^ Actual validation
       -> Validator m a  -- ^ Resulting validator
checkM error' f = Validator $ \x -> do
    valid <- f x
    return $ if valid then [] else [error']
