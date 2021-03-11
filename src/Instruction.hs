module Instruction (parseInstruction) where

import           Control.Applicative (Alternative ((<|>)))
import           Parser              (Parser, alphaString, char, direction,
                                      identifier, integer, oneOrMore, spaces,
                                      spaces', zeroOrMore)
import           Tape                (Direction, Symbol (Symbol))

------------------------------------------------
-- Instructions and its types
------------------------------------------------

-- | A `StateId` is an identifier (string)
newtype StateId = StateId String deriving (Eq)

-- | A `Value` is an integer (could be used as input/output)
data Value = IValue (Symbol Int) | SValue (Symbol String) deriving (Eq)

-- | An expression for the turing machine is given by
-- - A state name (state id)
-- - Something to read (value)
-- - A new state name (state id)
-- - Something to write (value)
-- - A movement (Direction)
data Instruction =
  Step
    { fromState    :: StateId
    , valueRead    :: Value
    , toState      :: StateId
    , valueWritten :: Value
    , dir          :: Direction
    }
  | Control
    { command :: String
    , value   :: [StateId]
    }
  deriving (Eq)

------------------------------------------------
-- Parsing a single instruction
------------------------------------------------

-- | Parses an instruction in the form  of a step such as `(fromState valueRead toState valueWritten dir)`
-- or in the form of a control such as `[NAME s1 s2 s3 ...]`
parseInstruction :: Parser Instruction
parseInstruction = parseStep <|> parseControl

-- | Parses a step in the such as `(s1 v1 s2 v2 dir)`
parseStep :: Parser Instruction
parseStep = delimiter '(' *> instruction <* delimiter ')'
  where
    instruction = Step
      <$> parseStateId <* spaces'
      <*> parseValue <* spaces'
      <*> parseStateId <* spaces'
      <*> parseValue <* spaces'
      <*> direction

-- | Parses a control sequence in the form `[NAME s1 s2 s3 ...]`
parseControl :: Parser Instruction
parseControl = delimiter '[' *> control <* delimiter ']'
  where
    control = Control
      <$> alphaString <* spaces'
      <*> values

    values = (:) <$> stateIdWithSpaces <*> zeroOrMore stateIdWithSpaces
    stateIdWithSpaces = spaces *> parseStateId <* spaces

-- | Parses a delimiter - a start or a stop sequence
delimiter :: Char -> Parser Char
delimiter c = spaces *> char c <* spaces

-- | Parses an atom
parseStateId :: Parser StateId
parseStateId = StateId <$> identifier

-- | Parses a value
parseValue :: Parser Value
parseValue = parseSValue <|> parseIValue
  where
    parseSValue = SValue . Symbol <$> identifier
    parseIValue = IValue . Symbol <$> integer

------------------------------------------------
-- Building a Map for all the instructions (TODO)
------------------------------------------------

------------------------------------------------
-- Instances
------------------------------------------------

instance Show StateId where
  show (StateId name) = name

instance Show Value where
  show (IValue (Symbol i)) = show i
  show (SValue (Symbol s)) = s

instance Show Instruction where
  show (Step s1 v1 s2 v2 d) = "(" ++ unwords [show s1, show v1, show s2, show v2, show d] ++ ")"
  show (Control name value) = "[" ++ unwords (name : map show value) ++ "]"
