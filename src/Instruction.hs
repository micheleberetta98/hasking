module Instruction
  ( Instruction(..)
  , parseInstruction
  , stateName
  , valueSymbol
  ) where

import           Control.Applicative (Alternative ((<|>)))
import           Data.Bifunctor
import           Data.Functor        (($>))
import           Parser              (Parser (runParser), alphaString, atom,
                                      char, direction, identifier, integer,
                                      oneOrMore, spaces, spaces1, zeroOrMore)
import           Tape                (Direction, Symbol (..))
import           TuringMachine

------------------------------------------------
-- Instructions and its types
------------------------------------------------

-- | A `Value` is an input/output on the tape (integer or string)
newtype Value = Value (Symbol String) deriving (Eq)

-- | An expression for the turing machine is given by
-- - A state name (state id)
-- - Something to read (value)
-- - A new state name (state id)
-- - Something to write (value)
-- - A movement (Direction)
data Instruction =
  Step
    { fromState    :: State
    , valueRead    :: Value
    , toState      :: State
    , valueWritten :: Value
    , dir          :: Direction
    }
  | Control
    { command :: String
    , value   :: [State]
    }
  deriving (Eq)

------------------------------------------------
-- Getting values
------------------------------------------------

stateName :: State -> String
stateName (State s) = s

valueSymbol :: Value -> Symbol String
valueSymbol (Value s) = s

------------------------------------------------
-- Parsing a single instruction
------------------------------------------------

-- | Parses an instruction in the form  of a step such as `(fromState valueRead toState valueWritten dir)`
-- or in the form of a control such as `[NAME s1 s2 s3 ...]`
-- parseInstruction :: String -> Instruction
parseInstruction :: String -> Maybe Instruction
parseInstruction s =
  case runParser (parseStep <|> parseControl) s of
    Just (i, "") -> Just i
    _            -> Nothing

-- | Parses a step in the such as `(s1 v1 s2 v2 dir)`
parseStep :: Parser Instruction
parseStep = delimiter '(' *> instruction <* delimiter ')'
  where
    instruction = Step
      <$> parseState <* spaces1
      <*> parseValue <* spaces1
      <*> parseState <* spaces1
      <*> parseValue <* spaces1
      <*> direction

-- | Parses a control sequence in the form `[NAME s1 s2 s3 ...]`
parseControl :: Parser Instruction
parseControl = delimiter '[' *> control <* delimiter ']'
  where
    control = Control
      <$> alphaString <* spaces1
      <*> values

    values = (:) <$> stateWithSpaces <*> zeroOrMore stateWithSpaces
    stateWithSpaces = spaces *> parseState <* spaces

-- | Parses a delimiter - a start or a stop sequence
delimiter :: Char -> Parser Char
delimiter c = spaces *> char c <* spaces

-- | Parses a state value
parseState :: Parser State
parseState = State <$> identifier

-- | Parses a value
parseValue :: Parser Value
parseValue = Value <$> parseSymbol
  where
    parseSymbol = parseBlank <|> (Symbol <$> atom)
    parseBlank = char '.' $> Blank

------------------------------------------------
-- Instances
------------------------------------------------

instance Show State where
  show (State name) = name

instance Show Value where
  show (Value (Symbol i)) = i
  show (Value Blank)      = "."

instance Show Instruction where
  show (Step s1 v1 s2 v2 d) = "(" ++ unwords [show s1, show v1, show s2, show v2, show d] ++ ")"
  show (Control name value) = "[" ++ unwords (name : map show value) ++ "]"
