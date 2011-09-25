-- | General functions for forms that are rendered to some sort of HTML
--
module Text.Digestive.Forms.Html
    ( FormHtmlConfig (..)
    , FormEncType (..)
    , FormHtml (..)
    , createFormHtml
    , createFormHtmlWith
    , viewHtml
    , mapViewHtml
    , applyClasses
    , defaultHtmlConfig
    , emptyHtmlConfig
    , renderFormHtml
    , renderFormHtmlWith
    ) where

import Data.Monoid (Monoid (..))
import Data.List (intercalate)
import Control.Applicative ((<*>), pure)
import Control.Arrow ((&&&))

import Text.Digestive.Types (Form, mapView, view)

-- | Settings for classes in generated HTML.
--
data FormHtmlConfig = FormHtmlConfig
    { htmlInputClasses     :: [String]  -- ^ Classes applied to input elements
    , htmlSubmitClasses    :: [String]  -- ^ Classes applied to submit buttons
    , htmlLabelClasses     :: [String]  -- ^ Classes applied to labels
    , htmlErrorClasses     :: [String]  -- ^ Classes applied to errors
    , htmlErrorListClasses :: [String]  -- ^ Classes for error lists
    } deriving (Show)

-- | Encoding type for the form
--
data FormEncType = UrlEncoded
                 | MultiPart
                 deriving (Eq)

instance Show FormEncType where
    show UrlEncoded = "application/x-www-form-urlencoded"
    show MultiPart  = "multipart/form-data"

-- Monoid instance for encoding types: prefer UrlEncoded, but fallback to
-- MultiPart when needed
instance Monoid FormEncType where
    mempty = UrlEncoded
    mappend UrlEncoded x = x
    mappend MultiPart _ = MultiPart

-- | HTML describing a form
--
data FormHtml a = FormHtml
    { formEncType :: FormEncType
    , formHtml    :: FormHtmlConfig -> a
    }

instance Monoid a => Monoid (FormHtml a) where
    mempty = FormHtml mempty $ const mempty
    mappend (FormHtml x f) (FormHtml y g) =
        FormHtml (x `mappend` y) $ f `mappend` g

instance Functor FormHtml where
    fmap f (FormHtml e g) = FormHtml e (f . g)

-- | Create form HTML with the default encoding type
--
createFormHtml :: (FormHtmlConfig -> a) -> FormHtml a
createFormHtml = FormHtml mempty

-- | Create form HTML with a custom encoding type
--
createFormHtmlWith :: FormEncType -> (FormHtmlConfig -> a) -> FormHtml a
createFormHtmlWith = FormHtml

-- | A shortcut for inserting HTML to the view, defined as a combination of
-- 'view' and 'createFormHtml'
--
viewHtml :: Monad m => a -> Form m i e (FormHtml a) ()
viewHtml = view . createFormHtml . const

-- | Lifted version of 'mapView'
--
mapViewHtml :: (Monad m, Functor m)
            => (v -> w)                   -- ^ Map over the contained HTML
            -> Form m i e (FormHtml v) a  -- ^ Initial form
            -> Form m i e (FormHtml w) a  -- ^ Resulting form
mapViewHtml f = mapView $ \(FormHtml e h) -> FormHtml e (f . h)

-- | Apply all classes to an HTML element. If no classes are found, nothing
-- happens.
--
applyClasses :: (a -> String -> a)            -- ^ Apply the class attribute
             -> [FormHtmlConfig -> [String]]  -- ^ Labels to apply
             -> FormHtmlConfig                -- ^ Label configuration
             -> a                             -- ^ HTML element
             -> a                             -- ^ Resulting element
applyClasses applyAttribute fs cfg element = case concat (fs <*> pure cfg) of
    []      -> element  -- No labels to apply
    classes -> applyAttribute element $ intercalate " " classes

-- | Default configuration
--
defaultHtmlConfig :: FormHtmlConfig
defaultHtmlConfig = FormHtmlConfig
    { htmlInputClasses     = ["digestive-input"]
    , htmlSubmitClasses    = ["digestive-submit"]
    , htmlLabelClasses     = ["digestive-label"]
    , htmlErrorClasses     = ["digestive-error"]
    , htmlErrorListClasses = ["digestive-error-list"]
    }

-- | Empty configuration (no classes are set)
--
emptyHtmlConfig :: FormHtmlConfig
emptyHtmlConfig = FormHtmlConfig
    { htmlInputClasses     = []
    , htmlSubmitClasses    = []
    , htmlLabelClasses     = []
    , htmlErrorClasses     = []
    , htmlErrorListClasses = []
    }

-- | Render FormHtml using the default configuration
--
renderFormHtml :: FormHtml a -> (a, FormEncType)
renderFormHtml = renderFormHtmlWith defaultHtmlConfig

-- | Render FormHtml using a custom configuration
--
renderFormHtmlWith :: FormHtmlConfig -> FormHtml a -> (a, FormEncType)
renderFormHtmlWith cfg = ($ cfg) . formHtml &&& formEncType
