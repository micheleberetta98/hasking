{-# LANGUAGE FlexibleInstances #-}
module Error
  ( Error(..)
  , ErrorType(..)
  , ErrorList
  , singleton
  , len
  , line
  , message
  ) where

import           Data.List (intercalate, sortOn)
import           Pretty    (Pretty (..))

------------------------------------------------
-- Data types
------------------------------------------------

-- | The possible errors
data ErrorType =
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

-- | An error can be a message with a line, or just a generic message
data Error = SimpleError ErrorType | LineError Int ErrorType
  deriving (Show, Eq)

-- | A list of errors
newtype ErrorList = ErrorList [Error] deriving (Show, Eq)

------------------------------------------------
-- Utils
------------------------------------------------

-- | Creates a list with a single error
singleton :: Error -> ErrorList
singleton le = ErrorList [le]

-- | Gets the number of errors in the `ErrorList`
len :: ErrorList -> Int
len (ErrorList es) = length es

-- | Retrieves the line of an `Error`
line :: Error -> Maybe Int
line (SimpleError _) = Nothing
line (LineError l _) = Just l

-- | Retrieves the message of an `Error`
message :: Error -> ErrorType
message (SimpleError msg) = msg
message (LineError _ msg) = msg

------------------------------------------------
-- Instances
------------------------------------------------

instance Semigroup ErrorList where
  (ErrorList e1) <> (ErrorList e2) = ErrorList (e1 ++ e2)

instance Monoid ErrorList where
  mempty = ErrorList []

instance Pretty ErrorType where
  pretty NoInstructions         = "No instructions provided"
  pretty NoInitialState         = "Missing initial state"
  pretty NoFinalStates          = "Missing final states"
  pretty MultiInitialState      = "Multiple initial states found"
  pretty MissingInputTape       = "Missing initial tape value"
  pretty EmptyInputTape         = "Empty initial tape value"
  pretty InvalidControlSequence = "Invalid control sequence, must be one of BEGIN or FINAL"
  pretty InvalidInstruction     = "Invalid instruction"
  pretty UnrecognizedChars      = "Unrecognized characters after the instruction"

instance Pretty Error where
  pretty (SimpleError s) = "(!) " ++ pretty s
  pretty (LineError l s) = "(!) at line " ++ pretty l ++ ": " ++ pretty s

instance Pretty ErrorList where
  pretty (ErrorList []) = ""
  pretty (ErrorList es) = intercalate "\n" . map pretty $ sortOn line es
