If you install `digestive-functors-blaze` and `digestive-functors-snap` from
Hackage, you should be good to go: run this file with `runghc` and you
should have a small webapp running at [localhost:8000](http://localhost:8000/).

We import `Text.Digestive` to get the general API provided by digestive
functors:

> {-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
> import Control.Applicative ((<$>), (<*>))
> import Text.Digestive

The digestive functors library is structured into three layers:

![The layered design](/images/2010-12-09-digestive-functors-layers.png)

For the actual web server responsible for I/O, we use
[Snap](http://snapframework.com). A [Happstack](http://happstack.com) backend is
available, too.

> import Text.Digestive.Forms.Snap
> import Snap.Types
> import Snap.Http.Server (httpServe, defaultConfig)

We use blaze as frontend. This is the only supported frontend for now, but we
are going to work on other frontends such as HSP.

> import Text.Digestive.Blaze.Html5
> import Text.Blaze (Html, toHtml, toValue, (!))
> import qualified Text.Blaze.Html5 as H
> import qualified Text.Blaze.Html5.Attributes as A
> import Text.Blaze.Renderer.Utf8 (renderHtml)

To illustrate use of the library, we will build a small webapp to calculate
a [weighted sum]. We have a simple datatype describing the input for our
mind-blowing calculations:

[weighted sum]: http://en.wikipedia.org/wiki/Weight_function

> data WeightedSum = WeightedSum [Double] [Double]

Of course, we also require a function for calculating the result:

> weightedSum :: WeightedSum -> Double
> weightedSum (WeightedSum weights values) = sum $ zipWith (*) weights values

To obtain the sum, we will need two lists, entered by the user. We will rely on the
property that list is an instance of `Read`.

> listForm :: (Read a, Show a, Monad m, Functor m)
>          => [a] -> SnapForm m Html BlazeFormHtml [a]
> listForm def = inputTextRead "Can't read list" (Just def) <++ errors

Let us examine this a little more closely.

~~~~~{.haskell}
listForm def =
~~~~~{.haskell}

We create a new Haskell function which returns a list. `def` is the default
value that the user will see when he accesses the web page.

~~~~~{.haskell}
inputTextRead "Can't read list" (Just def)
~~~~~

The above specifies a textbox for values instantiating `Read`. We give an error
message in case the user enters something invalid -- this error message will be
thrown when the value cannot be `read`. We also pass our default value.

~~~~~{.haskell}
<++ errors
~~~~~

`<++` is an operator used to append certain "special" forms on the right side
(of course, `++>` also exists). Here, we append `errors` -- this will basically
generate a list of errors for the corresponding field. Now we can look at the
type of the form:

~~~~~{.haskell}
SnapForm Html BlazeFormHtml [a]
~~~~~

This simply is a form using the Snap backend, using the `Html` type for the
errors (we use `Html` rather than `String` because we might want to have some
extra formatting in the errors). `BlazeFormHtml` is the "view" we produce,
and our form will return an `[a]`.

One of the main reasons for using applicative functors to create forms is
composability. We compose two `listForm`s into a form we can use for our
`WeightedSum` type. We compose using the standard `<$>` and `<*>` applicative
interface.

> weightedSumForm :: (Monad m, MonadSnap m)
>                 => SnapForm m Html BlazeFormHtml WeightedSum
> weightedSumForm = (`validate` equalSize) $ (<++ errors) $ WeightedSum
>     <$> label "Weights: " ++> listForm [0.4, 0.4, 0.2]
>     <*> label "Values: "  ++> listForm [64, 67, 91]

We use the `label` function here to create a semantic HTML `<label>` (when the
user clicks the label, the corresponding input field will be selected). We
`validate` our form using the `equalSize` validator (explained a bit further
down).

We also append `errors` to our `WeightedSum` form. The digestive functors
library has two main functions for selecting errors:

- `errors` lists only the errors corresponding to this exact form;
- `childErrors` lists all errors belonging to form, as well as all errors
  belonging to one of the children forms. In this case, using `childErrors`
  would mean that we would see "Can't read list" errors appearing twice (once
  for the `listForm`, and once for this form) -- but it can be quite useful in
  certain scenario's.

To calculate a weighted sum, the lists must be of the same size -- this is why
we have the `equalSize` validator. Writing validators is not very hard; this one
is particulary easy because it is a pure validator.

> equalSize :: Monad m => Validator m Html WeightedSum
> equalSize = check "Lists must be of equal size" $ \(WeightedSum l1 l2) ->
>     length l1 == length l2

With the `check` function, you simply provide an error message and a predicate,
and you are done.

Next, we need to get the webapp running on Snap. For this, the first thing we
require is a simple utility function to render our blaze templates:

> blaze :: Html -> Snap ()
> blaze response = do
>     modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
>     writeLBS $ renderHtml response

Second, we write a `Snap` handler to serve this form, as follows.

> weightedSumHandler :: Snap ()
> weightedSumHandler = do

The real digestive magic is provided by the `eitherSnapForm` function. It
evaluates the form on a `POST` request, and views the form on a `GET` requiest.

>     r <- eitherSnapForm weightedSumForm "weighted-sum-form"
>     case r of

Should we get a form back, either something went wrong, or the user only wishes
to view the form. In both cases, we simply render the form using blaze.

>         Left form' -> blaze $ do
>             let (formHtml', enctype) = renderFormHtml form'
>             H.style ! A.type_ "text/css" $ do
>                   "input {display: block;}\n"
>                   ".digestive-error-list {\n"
>                   "    color: white;\n"
>                   "    background-color: rgb(100, 0, 0);\n"
>                   "}"
>             H.h1 "Evaluate a weighted sum"
>             H.form ! A.enctype (toValue $ show enctype)
>                    ! A.method "POST" ! A.action "/" $ do
>                 formHtml'
>                 H.input ! A.type_ "submit" ! A.value "Submit"

Note how we also receive the encoding type (`enctype`) from the `renderFormHtml`
function. We use `.digestive-error-list` to style it up a little. Obviously,
these classes are completely customizable.

If we received an actual `WeightedSum`, it means that the user filled in
everything correctly, i.e., the input validated. We can now evaluate and print
this result.

>         Right weightedSum' -> blaze $ do
>             H.h1 "HUGE SUCCES"
>             H.p $ do
>                 H.strong "Result: "
>                 toHtml $ weightedSum weightedSum'

Finally, all we need to complete this example is a main function to server the
handler, and we are set!

> main :: IO ()
> main = httpServe defaultConfig weightedSumHandler
