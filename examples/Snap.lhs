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

We're going to make a small webapp to calculate a weighted sum. We have a
datatype describing the input:

> data WeightedSum = WeightedSum [Double] [Double]

And a function calculating the result:

> weightedSum :: WeightedSum -> Double
> weightedSum (WeightedSum l1 l2) = sum $ zipWith (*) l1 l2

Right, now we can start on the actual webapp. To calculate a weighted sum, it is
required that every value has a weight. This is why we require that the list of
weights has the same length as the list of values.

> equalSize :: Validator Snap Html WeightedSum
> equalSize = check "Lists must be of equal size" $ \(WeightedSum l1 l2) ->
>     length l1 == length l2

Now, we create a small form in which the user can enter a list (in Haskell
syntax).

> listForm :: (Read a, Show a) => [a] -> SnapForm Html BlazeFormHtml [a]
> listForm def = inputTextRead "Can't read list" (Just def) <++ errors

We compose two of these forms to create a `WeightedSum` form:

> weightedSumForm :: SnapForm Html BlazeFormHtml WeightedSum
> weightedSumForm = (`validate` equalSize) $ (<++ errors) $ WeightedSum
>     <$> label "Weights: " ++> listForm [0.4, 0.4, 0.2]
>     <*> label "Values: "  ++> listForm [64, 67, 91]

Some code to render blaze templates:

> blaze :: Html -> Snap ()
> blaze response = do
>     modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
>     writeLBS $ renderHtml response

Now, all we have to do is create a `Snap` handler to serve this form.

> weightedSumHandler :: Snap ()
> weightedSumHandler = do

Here, the digestive magic works. We will evaulate the form on a `POST` request,
and view the form on a `GET` request.

>     r <- eitherSnapForm weightedSumForm "weighted-sum-form"
>     case r of

If we get a form back, something went wrong, or the user just wants to view the
form. Either case, we just render the form using blaze.

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

If we got an actual `WeightedSum`, it means that the user filled in everything
correctly (the input validated). We can now print this result.

>         Right weightedSum' -> blaze $ do
>             H.h1 "HUGE SUCCES"
>             H.p $ do
>                 H.strong $ "Result: "
>                 H.string $ show $ weightedSum weightedSum'

Now, we just need a main function to server the handler.

> main :: IO ()
> main = httpServe "*" 8000 "weighted-sum" Nothing Nothing weightedSumHandler
