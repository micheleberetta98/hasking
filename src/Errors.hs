module Errors
  ( Error(..)
  , LineError
  , ErrorList
  , empty
  , justOne
  , simpleError
  , linedError
  , line
  , errorMsg
  , (.+)
  , (.++)
  ) where

import           Data.List (intercalate, sortOn)
import           Pretty    (Pretty (..))

------------------------------------------------
-- Data types
------------------------------------------------

-- | The possible errors
data Error =
  NoInstructions            -- The absence of any instruction in the form (s v s v d)
  | NoInitialState          -- No initial state has been specified
  | NoFinalStates           -- No final state has been specified
  | MultiInitialState       -- Multiple initial states have been specified
  | MissingInputTape        -- No input tape has been specified
  | EmptyInputTape          -- The input tape is empty
  | InvalidControlSequence  -- The control sequence is not one of the supported (ie. FINAL or BEGIN)
  | InvalidInstruction      -- The instruction could not be parsed
  | UnrecognizedChars       -- There are some characters after the instruction (that are not a comment)
  deriving (Show, Eq)

-- | An error message with a line (not required) associated
newtype LineError = LineError (Maybe Int, Error) deriving (Show, Eq)

-- | Represents a list of error messages with a line (not required) associated
newtype ErrorList = ErrorList [LineError] deriving (Show, Eq)

------------------------------------------------
-- Utils
------------------------------------------------

-- | An empty error list
empty :: ErrorList
empty = ErrorList []

-- | Creates a list with a single error
justOne :: LineError -> ErrorList
justOne le = ErrorList [le]

-- | Creates a simple error without a line
simpleError :: Error -> LineError
simpleError msg = LineError (Nothing, msg)

-- | Creates an error with a line
linedError :: Error -> Int -> LineError
linedError msg l = LineError (Just l, msg)

-- | Retrieves the line of a `LineError`
line :: LineError -> Maybe Int
line (LineError (l, _)) = l

-- | Retrieves the message of a `LineError`
errorMsg :: LineError -> Error
errorMsg (LineError (_, s)) = s

-- | Adds an error to the error list
(.+) :: ErrorList -> LineError -> ErrorList
es .+ le = es .++ justOne le

-- | Combines two ErrorList together
(.++) :: ErrorList -> ErrorList -> ErrorList
ErrorList es1 .++  ErrorList es2 = ErrorList (es1 ++ es2)

------------------------------------------------
-- Instances
------------------------------------------------

instance Pretty Error where
  pretty NoInstructions         = "No instructions provided"
  pretty NoInitialState         = "Missing initial state"
  pretty NoFinalStates          = "Missing final states"
  pretty MultiInitialState      = "Multiple initial states found"
  pretty MissingInputTape       = "Missing initial tape value"
  pretty EmptyInputTape         = "Empty initial tape value"
  pretty InvalidControlSequence = "Invalid control sequence, must be one of BEGIN or FINAL"
  pretty InvalidInstruction     = "Invalid instruction"
  pretty UnrecognizedChars      = "Unrecognized characters after the instruction"

instance Pretty LineError where
  pretty (LineError (Nothing, s)) = "(!) " ++ pretty s
  pretty (LineError (Just l, s))  = "(!) at line " ++ pretty l ++ ": " ++ pretty s

instance Pretty ErrorList where
  pretty (ErrorList []) = ""
  pretty (ErrorList es) = intercalate "\n" . map pretty $ sortOn line es
