Digestive functors
==================

Digestive functors is a library that provides an abstract interface towards
input consumption. The interface is based on applicative functors.

It is a rewrite and improvement of the existing
[formlets library](http://github.com/chriseidhof/formlets/).

It is mostly meant as a framework to generate HTML forms, and to process HTTP
POST requests into actual values. Some features include:

- complete control over errors and validation;
- forms are composable, thus allowing code re-use;

> {-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
> import Text.Digestive

We're going to use the [blaze-html](http://jaspervdj.be/blaze/) backend for
these examples.

> import Text.Digestive.Blaze.Html5
> import Text.Digestive.Forms (FormInput (..))
> import Text.Blaze (Html)
> import Text.Blaze.Renderer.Pretty (renderHtml)

Followed by some more general imports:

> import Control.Applicative

A simple example
----------------

We have a simple data structure for which we want to make an HTML form:

> data Address = Address
>     { addressLine   :: String
>     , addressCity   :: String
>     , addressPostal :: Int
>     } deriving (Show)

This is done quite easily by using the blaze backend. We use an applicative
style to write our code, which is quite easy on the eyes. More information on
applicative functors can be found
[here](http://learnyouahaskell.com/functors-applicative-functors-and-monoids).

> addressForm1 :: (Monad m, Functor m)
>              => Form m DummyInput Html BlazeFormHtml Address
> addressForm1 = Address <$> inputText Nothing
>                        <*> inputText (Just "Ghent")
>                        <*> inputTextRead "No read" (Just 9000)

We have a text input field for the address line, another one for the Ciry --
this one has a default value (Ghent) -- and then we have another text input
field returning an `Int`. A `inputTextRead` field can return any value that
instantiates `Read`. Here, the default value is `9000` and if the value cannot
be parsed properly, `"No Read"` will be given as error.

We can use the `eitherForm` function to actually run the form. This function
takes the form to run, an identifier and an environment from which the input is
consumed.

Basically, if we're talking about a REST application:

- for a `GET`, you would use a `NoEnvironment` value (since there is no data);
- for a `POST`, you would create an `Environment` which has a lookup table for
  the post parameters.

The `eitherForm` will then either return a view or a value:

- if a value is returned, it means that there were no errors, and you can
  safely continue your application using this value;
- if a view was returned, it means that some error occurred -- this error should
  be visible in the view, so you can use the view for feedback to the user.

We can now declare a `DummyInput` type and a `testForm` function to test a form
locally using GHCI. Note that this sort of code normally isn't necesarry: all
instances should be provided for common Haskell web frameworks.

> newtype DummyInput = DummyInput {unDummyInput :: String}

> instance FormInput DummyInput [Char] where
>     getInputStrings = return . unDummyInput
>     getInputFile = Just . unDummyInput

> testForm :: Show a
>          => Form IO DummyInput Html BlazeFormHtml a
>          -> [(Integer, String)]
>          -> IO ()
> testForm form tuples = eitherForm form "some-form" env >>= \er ->
>     putStrLn $ case er of
>         Left html -> renderHtml $ fst $ renderFormHtml html
>         Right x -> show x
>   where
>     env = Environment $
>         return . fmap DummyInput . flip lookup tuples . head . formIdList

Try loading this file in GHCI and using

    testForm addressForm1 [(0, "Hoveniersberg 24"), (1, "Ghent"), (2, "9000")]
    testForm addressForm1 []

Adding labels
-------------

HTML provides a semantic `<label>` element that you can use to make your forms
more descriptive. However, this is not completely trivial: you want to link the
labels to the input elements using the `for` attribute.

    <label for="some-id" ...>...</label>
    <input id="some-id" ... />

This is correct from a GUI point of view, since it allows the browser to make
the form more user-friendly (e.g. when the user clicks a label, the
corresponding input field receives focus).

Digestive functors provides this functionality using the `++>` and `<++`
operators. These allow you to add forms which do not give any result (they
return `()`) to other forms. While these forms do not return any result, they
are allowed to change the view.

Let's make our `addressForm1` a little more user-friendly:

> addressForm2 :: (Monad m, Functor m)
>              => Form m DummyInput Html BlazeFormHtml Address
> addressForm2 = Address
>     <$> label "Address line: " ++> inputText Nothing
>     <*> label "City: " ++> inputText (Just "Ghent")
>     <*> label "Postal code: " ++> inputTextRead "No read" (Just 9000)

Try having a look at the generated HTML.

You can also add labels to the right side of elements using the `<++` operator.

Validation
----------

The `Text.Digestive.Validate` module gives us some primitves for using
validation on forms. Say that we only want to accept postal codes in the range
`[9000 .. 9999]`. We can create a validator using the `check` function:

> inRange :: Monad m => Validator m Html Int
> inRange = check "Must be in the range [9000 .. 9999]" $ \x ->
>     x >= 9000 && x <= 9999

We can attach this error to the form using the `validate` function.

> addressForm3 :: (Monad m, Functor m)
>              => Form m DummyInput Html BlazeFormHtml Address
> addressForm3 = Address
>     <$> inputText Nothing
>     <*> inputText (Just "Ghent")
>     <*> inputTextRead "No read" (Just 9000) `validate` inRange

You can see that the form fails using GHCI, but how do we actually view the
error?

> addressForm4 :: (Monad m, Functor m)
>              => Form m DummyInput Html BlazeFormHtml Address
> addressForm4 = (++>) childErrors $ Address
>     <$> inputText Nothing
>     <*> inputText (Just "Ghent")
>     <*> inputTextRead "No read" (Just 9000) `validate` inRange

If you test `addressForm4` in GHCI, you will see that the errors are listed
nicely before the actual form HTML.

Errors
------

Errors can originate from an input field. For example, when we have an
`inputTextRead` field for an integer, the parsing will fail when the user fills
in "BOOYAAAH", and the error will originate directly from that input field.

We can add all errors relating to the fields in the previous form:

> addressForm5 :: (Monad m, Functor m)
>              => Form m DummyInput Html BlazeFormHtml Address
> addressForm5 = Address
>     <$> inputText Nothing                                      <++ errors
>     <*> inputText (Just "Ghent")                               <++ errors
>     <*> inputTextRead "No read" (Just 9000) `validate` inRange <++ errors

Now, every field will have an error list after it, showing only the directly
relevant errors.

However, suppose we have a form for a Hostel called "The Lambda Hostel". Guests
can reserve a room by filling out a form on the Hostel website, and they have to
fill in the arrival and departure dates.

It's obvious that, until the invention of a time machine, the arrival date will
have to be before the departure date. We're going to use simple `Int`s for
dates, to keep the examples simple.

> data Booking = Booking Int Int
>              deriving (Show)

> bookingForm1 :: (Monad m, Functor m)
>              => Form m DummyInput Html BlazeFormHtml Booking
> bookingForm1 = Booking <$> inputTextRead "No read" Nothing
>                        <*> inputTextRead "No read" Nothing

A simple validator to ensure the validity of bookings:

> correctBooking :: Monad m => Validator m Html Booking
> correctBooking = check "Time machine detected!" $ \(Booking x y) -> x < y

Now, we can use this validator on the booking form:

> bookingForm2 :: (Monad m, Functor m)
>              => Form m DummyInput Html BlazeFormHtml Booking
> bookingForm2 = bookingForm1 `validate` correctBooking <++ errors

Congratulations, we now have a working time machine detector! However, the
errors arrising from the `inputTextRead` are nowhere to be found now -- because
we didn't use `errors` in `bookingForm1`.

That's why the `childErrors` exists.

> bookingForm3 :: (Monad m, Functor m)
>              => Form m DummyInput Html BlazeFormHtml Booking
> bookingForm3 = bookingForm1 `validate` correctBooking <++ childErrors

This will give a listing of all errors regarding `bookingForm1` and all fields
which are children of `bookingForm1`, when representing the form as a tree
structure:

    bookingForm1
    |- inputTextRead
    \- inputTextRead
