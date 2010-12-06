-- | Functions to construct common forms
--
module Text.Digestive.Common
    ( input
    , label
    , errors
    , childErrors
    ) where

import Text.Digestive.Types
import Text.Digestive.Result

-- TODO: Check parameter usage
input :: (Monad m, Functor m)
      => (Bool -> Maybe i -> d -> s)           -- ^ Get the viewed result
      -> (Maybe i -> FormRange -> Result e a)  -- ^ Get the returned result
      -> (FormId -> s -> v)                    -- ^ View constructor
      -> d                                     -- ^ Default value
      -> Form m i e v a                        -- ^ Resulting form
input toView toResult createView defaultInput = Form $ do
    isInput <- isFormInput
    inp <- getFormInput
    id' <- getFormId
    range <- getFormRange
    let view' = toView isInput inp defaultInput
        result' = toResult inp range
    return (View (const $ createView id' view'), result')

label :: Monad m
      => (FormId -> v)
      -> Form m i e v ()
label f = Form $ do
    id' <- getFormId
    return (View (const $ f id'), Ok ())

errors :: Monad m
       => ([e] -> v)
       -> Form m i e v ()
errors f = Form $ do
    range <- getFormRange
    return (View (f . retainErrors range), Ok ())

childErrors :: Monad m
            => ([e] -> v)
            -> Form m i e v ()
childErrors f = Form $ do
    range <- getFormRange
    return (View (f . retainChildErrors range), Ok ())
