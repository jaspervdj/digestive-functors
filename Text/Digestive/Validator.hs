-- | Validators that can be attached to forms
--
module Text.Digestive.Validator
    ( Validator
    , validate
    , validateMany
    , check
    , checkM
    ) where

import Prelude hiding (id)

import Control.Monad (liftM2)
import Data.Monoid (Monoid (..))
import Control.Category (id)

import Text.Digestive.Types
import Text.Digestive.Transformer

-- | A validator. Invariant: the validator should not modify the result value,
-- only check it.
--
newtype Validator m e a = Validator {unValidator :: Transformer m e a a}

instance Monad m => Monoid (Validator m e a) where
    mempty = Validator id
    v1 `mappend` v2 = Validator $ Transformer $ \inp ->
        liftM2 eitherPlus (unTransformer (unValidator v1) inp)
                          (unTransformer (unValidator v2) inp)
      where
        eitherPlus (Left e) (Left i) = Left $ e ++ i
        eitherPlus (Left e) (Right _) = Left e
        eitherPlus (Right _) (Left e) = Left e
        eitherPlus (Right a) (Right _) = Right a

-- | Attach a validator to a form.
--
validate :: Monad m => Form m i e v a -> Validator m e a -> Form m i e v a
validate form = transform form . unValidator

-- | Attach multiple validators to a form.
--
validateMany :: Monad m => Form m i e v a -> [Validator m e a] -> Form m i e v a
validateMany form = validate form . mconcat

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
checkM error' f = Validator $ Transformer $ \x -> do
    valid <- f x
    return $ if valid then Right x else Left [error']
