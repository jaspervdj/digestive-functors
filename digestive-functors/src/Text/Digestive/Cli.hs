-- | Proof-of-concept module: use digestive functors for a command line
-- interface prompt
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Digestive.Cli
    ( Prompt
    , prompt
    , promptList
    , promptRead
    , runPrompt
    ) where

import Data.Monoid (Monoid, mappend, mempty)
import Control.Applicative ((<$>))

import Text.Digestive.Result
import Text.Digestive.Types
import Text.Digestive.Transform
import Text.Digestive.Forms (inputList)

-- A representation of an element in the structure used to gather inputs
-- from the user.
--
data FieldItem
  -- A tangible item that the user is prompted for.
  = FieldItemSingle     FormId String [String]
  -- A delimter marking the start of a series of prompts to be entered
  -- multiple times.
  | FieldItemMultiStart FormId String [String]
  -- A delimiter marking the end of a multiple input prompt.
  | FieldItemMultiEnd
    deriving (Show)

-- The structure of a prompt, built up as a View.
--
newtype PromptView = PromptView
    { unPromptView :: [FieldItem]
    } deriving (Show, Monoid)

-- | Type for a prompt
--
type Prompt a = Form IO String String PromptView a

-- An association list of FormIds and their inputs gathered by prompting the
-- user.
--
newtype InputMap = InputMap
    { unInputMap :: [(FormId, String)]
    } deriving (Show, Monoid)

-- Create an environment from an input map
--
inputMapEnvironment :: Monad m => InputMap -> Environment m String
inputMapEnvironment map' = Environment $ return . flip lookup (unInputMap map')

-- | Generate a prompt field for a String
--
prompt :: String        -- ^ Description
       -> Prompt String -- ^ Resulting prompt
prompt descr = Form $ do
    id' <- getFormId
    inp <- getFormInput
    let v :: [(FormRange, String)] -> PromptView
        v errs = PromptView [FieldItemSingle id' descr (map snd matching)]
          where
            -- Only errors which apply specifically to this item
            matching :: [(FormRange, String)]
            matching = filter (flip isSubRange range . fst) errs
        range = unitRange id'
        result = case inp of
          Just x  -> Ok x
          Nothing -> Error [(range, "No input")]
    return (View v, result)

-- | Convert a prompt for a single item into a prompt for multiple items
--
promptList :: String     -- ^ Description of resulting multi-prompt
           -> Prompt a   -- ^ Prompt to convert
           -> Prompt [a] -- ^ Resulting multiple input prompt
promptList descr prmpt = Form $ do
    id'     <- getFormId
    (v, r1) <- unForm $ inputList numPrompt (const prmpt) Nothing
    range   <- getFormRange
    -- The monoid for the view will look like this: [vstart, v, vend]
    -- 'vstart' and 'vend' delimit the beginning and end of the inputs that
    -- are converted into repeatable inputs. When we are prompting the user to
    -- fill out the form, we use these delimiters to control the behavior of
    -- the prompts. Anything between them will be treated as repeatable. The
    -- 'vstart' item (FieldItemMultiStart) also contains the FormId of the
    -- field used to count the number of entries, as well as an additional
    -- description of of the multiple input itself (for example, 'Users', when
    -- the contained items are used to enter in a single User.)
    let vstart errs = PromptView [item]
          where
            item = FieldItemMultiStart id' descr (map snd matching)
            matching = filter (flip isSubRange range . fst) errs
        vend _ = PromptView [FieldItemMultiEnd]
    return (View vstart `mappend` v `mappend` View vend, r1)
  where
    numPrompt _ = Form $ do
                    inp <- getFormInput
                    return (mempty, readN inp)
    readN (Just x) = Ok (read x)
    readN Nothing = Error []

-- | Generate a prompt field for a value which can be read
--
promptRead :: Read a
           => String   -- ^ Error when the value can't be read
           -> String   -- ^ Description
           -> Prompt a -- ^ Resulting prompt
promptRead error' descr = prompt descr `transform` transformRead error'

-- Get a single line of text from the user
--
cliInput :: IO String
cliInput = putStr "> " >> getLine

-- Get the input for a list of prompt items that have been defined.
--
-- Notably, this supports nested 'mass input' forms (inputList/promptList)
-- which are delimited by FieldItemMultiStart and FieldItemMultiEnd.
-- FieldItemMultiSingle represents a tangible item to prompt for. If a
-- FieldItemMultiStart is reached, we need to prompt for all of the items
-- until the next FieldItemMultiEnd an arbitrary number of times.
--
inputForItems :: [FieldItem]
              -- ^ Items to get input for
              -> [(FormId, String)]
              -- ^ Accumulated association list of inputs we've prompted for
              -> (FormId -> FormId)
              -- ^ A function to transform the FormId of this item. Used to
              -- change the index of the item when prompting multiple times.
              -> IO ([FieldItem], [(FormId, String)])
              -- ^ A pair of the remaining items (empty if we are not
              -- returning from a multiple input item) and accumulated inputs, 
inputForItems [] accum _fid = return ([], accum)

inputForItems (FieldItemMultiEnd : rest) accum _fid = return (rest, accum)

-- The simple case for a single item.
inputForItems (FieldItemSingle id' descr _errs : rest) accum fid = do
    putStrLn descr
    val <- cliInput
    inputForItems rest ((fid id', val) : accum) fid

-- The case for a multiple input prompt.
inputForItems (FieldItemMultiStart id' descr _errs : rest) accum fid = do
    let id'' = fid id'
    putStrLn $ "How many '" ++ descr ++ "' do you want to input?"
    -- Leave this as a string, since we must put it into a hidden form field
    -- for inputList, which must read it again. We only prompt for it here
    -- instead of as a discrete form item because we want to know, right now,
    -- how many the user wants to input.
    nStr <- cliInput
    -- Prompt for all of the delimited items, and put them at index i for this
    -- multi-input item.
    -- TODO use foldM
    let f i = do putStrLn $ descr ++ " #" ++ show (i + 1) ++ ":"
                 inputForItems rest [] (modifyId id'' i)
    delimited <- mapM f [0..(read nStr - 1)]
    let rest' = fst $ last delimited
        countfield = (id'', nStr)
    inputForItems rest' ([countfield] ++ accum ++ concatMap snd delimited) fid

-- Construct a function to transform the 'children' (delimited items) of a
-- mass input item to the correct index.
--
modifyId :: FormId -> Integer -> FormId -> FormId
modifyId parent i = mapFormId (\x -> head x : i : formIdList parent)

-- Apply a function to the [Integer] field of a FormId
--
mapFormId :: ([Integer] -> [Integer]) -> FormId -> FormId
mapFormId f x = x {formIdList = f (formIdList x)}

-- | Run a Prompt, sequentially prompting the user for each item.
--
runPrompt :: Prompt a
          -- ^ The Prompt to run
          -> IO (Either [String] a)
          -- ^ A list of error strings, or the result.
runPrompt prmpt = do
    prmptv <- viewForm prmpt "form"
    inpmap <- InputMap . snd <$> inputForItems (unPromptView prmptv) [] id
    eith   <- eitherForm prmpt "form" (inputMapEnvironment inpmap)
    return $ case eith of
      Left v  -> Left (fieldItemErrors `concatMap` unPromptView v)
      Right x -> Right x

-- Read the errors from a FieldItem, if any.
--
fieldItemErrors :: FieldItem -> [String]
fieldItemErrors (FieldItemSingle id' descr errs) =
    descriptiveErrors id' descr errs
fieldItemErrors (FieldItemMultiStart id' descr errs) = -- TODO bad
    descriptiveErrors id' descr errs
fieldItemErrors FieldItemMultiEnd = []

descriptiveErrors :: FormId -> String -> [String] -> [String]
descriptiveErrors id' descr errs = map str errs
  where
    str err = "(" ++ show id' ++ ") " ++ descr ++ ": " ++ err


