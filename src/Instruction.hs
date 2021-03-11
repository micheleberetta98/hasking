module Instruction (parseInstruction) where

import           Control.Applicative (Alternative ((<|>)))
import           Data.Functor        (($>))
import           Parser              (Parser, alphaString, char, direction,
                                      identifier, integer, oneOrMore, spaces,
                                      spaces1, zeroOrMore)
import           Tape                (Direction, Symbol (..))

------------------------------------------------
-- Instructions and its types
------------------------------------------------

-- | A `StateId` is an identifier (string)
newtype StateId = StateId String deriving (Eq)

-- | A `Value` is an input/output on the tape (integer or string)
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
      <$> parseStateId <* spaces1
      <*> parseValue <* spaces1
      <*> parseStateId <* spaces1
      <*> parseValue <* spaces1
      <*> direction

-- | Parses a control sequence in the form `[NAME s1 s2 s3 ...]`
parseControl :: Parser Instruction
parseControl = delimiter '[' *> control <* delimiter ']'
  where
    control = Control
      <$> alphaString <* spaces1
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
    parseSValue = SValue <$> parseSymbol identifier
    parseIValue = IValue <$> parseSymbol integer

    parseSymbol contentParser = parseBlank <|> (Symbol <$> contentParser)
    parseBlank = char '.' $> Blank

------------------------------------------------
-- Instances
------------------------------------------------

instance Show StateId where
  show (StateId name) = name

instance Show Value where
  show (IValue (Symbol i)) = show i
  show (SValue (Symbol s)) = s
  show (SValue Blank)      = "."
  show (IValue Blank )     = "."

instance Show Instruction where
  show (Step s1 v1 s2 v2 d) = "(" ++ unwords [show s1, show v1, show s2, show v2, show d] ++ ")"
  show (Control name value) = "[" ++ unwords (name : map show value) ++ "]"
