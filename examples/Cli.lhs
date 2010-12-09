Example illustrating the use of command-line input with digestive-functors

> module Main where

> import Control.Applicative ((<$>), (<*>))
> import Data.Char (isLower)

> import Text.Digestive.Validate

The provided functions for command-line input are located in this module:

> import Text.Digestive.Cli

We have a simple data type...

> data User = User
>     { userName :: String
>     , userAge  :: Int
>     } deriving (Show)

For which we create a prompt. We only accept lowercase names.

> userPrompt :: Prompt User
> userPrompt = User
>     <$> prompt "Name" `validate` check "Must be lower" (all isLower)
>     <*> promptRead "Can't read age" "Age"

We can now simply run the prompt using the `runPrompt` function:

> main :: IO ()
> main = do
>     user <- runPrompt userPrompt
>     putStrLn $ show user
