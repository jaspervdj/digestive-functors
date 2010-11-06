-- | General functions for forms that are rendered to some sort of HTML
module Text.Digestive.Html
    ( FormHtmlConfig (..)
    , FormHtml (..)
    , applyClasses
    , defaultHtmlConfig
    , emptyHtmlConfig
    , renderFormHtml
    , renderFormHtmlWith
    ) where

import Data.Monoid (Monoid (..))
import Data.List (intercalate)
import Control.Applicative ((<*>), pure)

-- | Settings for classes in generated HTML.
--
data FormHtmlConfig = FormHtmlConfig
    { htmlInputClasses :: [String]      -- ^ Classes applied to input elements
    , htmlLabelClasses :: [String]      -- ^ Classes applied to labels
    , htmlErrorClasses :: [String]      -- ^ Classes applied to errors
    , htmlErrorListClasses :: [String]  -- ^ Classes for error lists
    } deriving (Show)

-- | HTML describing a form
--
newtype FormHtml a = FormHtml
    { unFormHtml :: FormHtmlConfig -> a
    }

instance Monoid a => Monoid (FormHtml a) where
    mempty = FormHtml $ const mempty
    mappend (FormHtml x) (FormHtml y) = FormHtml $ \c -> mappend (x c) (y c)

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
    { htmlInputClasses = ["digestive-input"]
    , htmlLabelClasses = ["digestive-label"]
    , htmlErrorClasses = ["digestive-error"]
    , htmlErrorListClasses = ["digestive-error-list"]
    }

-- | Empty configuration (no classes are set)
--
emptyHtmlConfig :: FormHtmlConfig
emptyHtmlConfig = FormHtmlConfig
    { htmlInputClasses = []
    , htmlLabelClasses = []
    , htmlErrorClasses = []
    , htmlErrorListClasses = []
    }

-- | Render FormHtml using the default configuration
--
renderFormHtml :: FormHtml a -> a
renderFormHtml = renderFormHtmlWith defaultHtmlConfig

-- | Render FormHtml using a custom configuration
--
renderFormHtmlWith :: FormHtmlConfig -> FormHtml a -> a
renderFormHtmlWith cfg = ($ cfg) . unFormHtml
