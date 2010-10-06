Digestive Functors
==================

Introduction
------------

> import Text.Digestive.Types
> import Text.Digestive.Validator
> import Text.Digestive.Blaze.Html5
> import Text.Blaze (Html)

Digestive Functors is a Haskell framework/library that provides a general way to
create forms, based on *idioms*, or *applicative functors*. It is an improvement
of the original formets[^formlets] in a number of ways.

[^formlets]: By formlets, we mean the Haskell `formlets` library by Chris
    Eidhof, based on [http://groups.inf.ed.ac.uk/links/formlets/]().

TODO: Insert references to chris's formlets package, and the original paper

It differs from the original *formlets* package in a number of ways. Important
benefits of our work is that:

- Instead of just producing errors, the errors have a reference to the original
  input field (or composition of input fields) where they originate from. This
  allows us to show the relevant errors directly next to the input field, which
  is desirable from a GUI-perspective.

- We aim to provide functions with which the resulting "view" can be easily
  changed. This way, the developer using the library can refer to certain input
  fields, which allows him to refer to these fields in, for example, additional
  JavaScript code.

- While HTML forms remains the main focus, we do not want to be limited to it.
  Another backend could, for example, provide a command-line input prompt.

A note on terminology: with "input field", we mean a **single** input field,
the visual representation of a single HTML `<input>` element. With "form" we
mean a composition of input fields -- possibly, this is a single field, but
usually, a form will be composed of multiple fields.

Applicative functors
--------------------

> import Control.Applicative ((<$>), (<*>))

This section explains how applicative functors are used in the library, and we
explain why applicative functors are an excellent way to represent HTML forms.
If you are familiar with the old formlets package, you can skip this section.

Applicative funtors usually map very well onto Haskell datatype constructors.
Given the following type, which represents the name and age of a user:

> data User = User String Integer

We have a function which returns serializable values from a key-value store such
as [redis]:

> storeGet :: Read a => String -> IO a
> storeGet key = fail "Not implemented"

We can now use the fact that `IO` is an applicative functor to create a function
which constructs a `User` in `IO`:

> getUser :: IO User
> getUser = User <$> storeGet "user_name"
>                <*> storeGet "user_age"

We can conclude that using applicative functors to construct values result in
very readable and concise code.

The `getUser` example used above is very similar to the way we would use HTML
forms -- because it's an applicative functor, too.

> userForm :: Form IO String String Html User
> userForm = User <$> inputString Nothing
>                 <*> inputRead (Just 20)

Don't let the complicated type of `userForm` scare you: it's just a `Form`
returning a `User` -- we will see the details later. We give no default value
(`Nothing`) to the username field, and 20 (`Just 20`) as default value to the
age field.

Composing forms
---------------

The advantage of using applicative functors to create forms over classical
approaches is composability. For example, if we want to create a form in which
you can enter a couple, we can easily reuse our `userForm`.

> data Couple = Couple User User

> coupleForm :: Form IO String String Html Couple
> coupleForm = Couple <$> userForm
>                     <*> userForm

Validation
----------

In Belgium, people can only marry once they have reached the age of 18 -- and
our clients wants us to integrate this into our web application. This is a
simple example of validation.

> isAdult :: Validator IO String User
> isAdult = check "Not an adult!" $ \(User _ age) -> age >= 18

Once we have constructed this `Validator`, we can integrate it with our form:

> coupleForm' :: Form IO String String Html Couple
> coupleForm' = Couple <$> userForm `validate` [isAdult]
>                      <*> userForm `validate` [isAdult]

Note that we insert this validator in the couple form, not in the user form --
we allow users under 18, they just cannot belong to a couple.

Suppose an end user fills in the form. However, he tries to register a 16-year
old user in a couple. The validation does not allow this, so we will receive the
"Not an adult!" error.

However, the end user filled in two users. If only one of them is underage, we
want to show the error next to the form of the underage user, not next to the
form of the valid user. How can we do this?

Tracing errors
--------------

There is always some sort of ID associated with every input field. This is a
prerequisite of any form library -- our server will receive something like:

    POST / HTTP/1.1
    Content-Length: 23
    Content-Type: application/x-www-form-urlencoded
    
    field1=jasper&field2=20

If we had no ID associated with the input fields, we cannot construct a `User`,
since we do not know if "jasper" is the username or the age.

This allows us to do basic error tracing: if we associate an ID with the error,
we can trace it back to the corresponding input field. While this allows us to
do error tracing on *input fields*, it does not allow us to do error tracing on
*forms*. Forms, by default, have no ID -- they are a composition of input
fields. How can we represent this in our `Form` type?
