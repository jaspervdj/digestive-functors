-- | Validators that can be attached to forms
--
module Text.Digestive.Validator where

import Control.Monad (liftM2)
import Control.Monad.Trans (lift)
import Data.Monoid (Monoid (..))

import Text.Digestive.Types

-- | A validator
--
newtype Validator m e a = Validator {unValidator :: a -> m [e]}

instance Monad m => Monoid (Validator m e a) where
    mempty = Validator $ const $ return mempty
    v1 `mappend` v2 = Validator $ \inp ->
        liftM2 mappend (unValidator v1 inp) (unValidator v2 inp)

-- | Attach 'Validator's to a 'Form'
--
validate :: Monad m => Form m i e v a -> [Validator m e a] -> Form m i e v a
validate form validators = Form $ do
    (view', result) <- unForm form
    range <- getFormRange
    -- Check the result
    case result of
        -- We already have an error, no more validation needed
        Error e -> return (view', Error e)
        NoResult -> return (view', NoResult)
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
      => e                -- ^ Error message
      -> (a -> Bool)      -- ^ Actual validation
      -> Validator m e a  -- ^ Resulting validator
check error' = checkM error' . (return .)

-- | Easy way to create a monadic validator
--
checkM :: Monad m
       => e                -- ^ Error message
       -> (a -> m Bool)    -- ^ Actual validation
       -> Validator m e a  -- ^ Resulting validator
checkM error' f = Validator $ \x -> do
    valid <- f x
    return $ if valid then [] else [error']
