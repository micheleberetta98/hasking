{-# LANGUAGE FlexibleInstances #-}
module Error
  ( Error(..)
  , ErrorType(..)
  ) where

import           Data.List (sortOn)
import           Pretty    (Pretty (..), prettyList')

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
data Error =
  SimpleError ErrorType
  | LineError Int ErrorType
  deriving (Show, Eq)

------------------------------------------------
-- Instances
------------------------------------------------

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

instance Pretty [Error] where
  pretty [] = ""
  pretty es = prettyList' "\n" $ sortOn line es
    where
      line (SimpleError _) = Nothing
      line (LineError l _) = Just l
