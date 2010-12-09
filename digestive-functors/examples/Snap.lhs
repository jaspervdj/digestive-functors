Example illustrating snap with digestive-functors. This requires the
digestive-functors-snap package.

> {-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
> import Control.Applicative ((<$>), (<*>))
> import Snap.Types
> import Snap.Http.Server (httpServe)

> import Text.Blaze (Html, (!))
> import qualified Text.Blaze.Html5 as H
> import qualified Text.Blaze.Html5.Attributes as A
> import Text.Blaze.Renderer.Utf8 (renderHtml)

> import Text.Digestive.Types
> import Text.Digestive.Blaze.Html5
> import Text.Digestive.Forms.Snap
> import Text.Digestive.Validate

> data WeightedSum = WeightedSum [Double] [Double]

> weightedSum :: WeightedSum -> Double
> weightedSum (WeightedSum l1 l2) = sum $ zipWith (*) l1 l2

> equalSize :: Validator Snap Html WeightedSum
> equalSize = check "Lists must be of equal size" $ \(WeightedSum l1 l2) ->
>     length l1 == length l2

> listForm :: (Read a, Show a) => SnapForm Html BlazeFormHtml [a]
> listForm = inputTextRead "Can't read list" (Just []) <++ errors

> weightedSumForm :: SnapForm Html BlazeFormHtml WeightedSum
> weightedSumForm = (`validate` equalSize) $ (<++ errors) $ WeightedSum
>     <$> label "Weights: " ++> listForm
>     <*> label "Values: " ++> listForm

Some code to render blaze templates

> blaze :: Html -> Snap ()
> blaze response = do
>     modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
>     writeLBS $ renderHtml response

> weightedSumHandler :: Snap ()
> weightedSumHandler = do
>     r <- eitherSnapForm weightedSumForm "weighted-sum-form"
>     case r of
>         Left form' -> blaze $ do
>             let (formHtml', enctype) = renderFormHtml form'
>             H.style ! A.type_ "text/css" $ do
>                   "input {display: block;}\n"
>                   ".digestive-error-list {\n"
>                   "    color: white;\n"
>                   "    background-color: rgb(100, 0, 0);\n"
>                   "}"
>             H.h1 "Evaluate a weighted sum"
>             H.form ! A.enctype (H.stringValue $ show enctype)
>                    ! A.method "POST" ! A.action "/" $ do
>                 formHtml'
>                 H.input ! A.type_ "submit" ! A.value "Submit"
>         Right weightedSum' -> blaze $ do
>             H.h1 "HUGE SUCCES"
>             H.p $ do
>                 H.strong $ "Result: "
>                 H.string $ show $ weightedSum weightedSum'

> main :: IO ()
> main = httpServe "*" 8000 "weighted-sum" Nothing Nothing weightedSumHandler
