Example illustrating the use of command-line input with digestive-functors

> module Main where

> import Control.Applicative ((<$>), (<*>))
> import Data.Char (isLower)

> import Text.Digestive.Validate

The provided functions for command-line input are located in this module:

> import Text.Digestive.Cli

We have a simple data type...

> data User = User
>     { userName    :: String
>     , userAge     :: Int
>     , userHobbies :: [String]
>     } deriving (Show)

For which we create a prompt. We only accept lowercase names. A User can have
multiple hobbies, so we transform a prompt for a single hobby into a list of
such.

> userPrompt :: Prompt User
> userPrompt = User
>     <$> prompt "Name" `validate` check "Must be lower" (all isLower)
>     <*> promptRead "Can't read age" "Age"
>     <*> promptList "Hobbies" (prompt "Hobby")

We'll also prompt for multiple users. We can run the resulting prompt using
the `runPrompt` function:

> main :: IO ()
> main = do
>     result <- runPrompt (promptList "Users" userPrompt)
>     case result of
>         Left errors -> do
>             putStrLn "Sadly, there were errors with your form input"
>             mapM_ putStrLn errors
>         Right users -> putStrLn (show users)
