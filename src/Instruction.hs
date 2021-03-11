module Instruction (parseInstruction) where

import           Control.Applicative (Alternative ((<|>)))
import           Parser              (Parser, char, direction, identifier,
                                      integer, spaces, spaces')
import           Tape

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
data Instruction = Instruction
  { fromState    :: StateId
  , valueRead    :: Value
  , toState      :: StateId
  , valueWritten :: Value
  , dir          :: Direction
  } deriving (Eq)

------------------------------------------------
-- Parsing a single instruction
------------------------------------------------

-- | Parses an instruction in the form `(fromState valueRead toState valueWritten dir)`
parseInstruction :: Parser Instruction
parseInstruction = start *> instruction <* stop
  where
    start = spaces *> char '(' *> spaces
    stop = spaces <* char ')' <* spaces

    instruction = Instruction <$> s1 <* seps <*> v1 <* seps <*> s2 <* seps <*> v2 <* seps <*> direction

    s1 = parseStateId
    s2 = parseStateId
    v1 = parseValue
    v2 = parseValue

    seps = spaces'

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
  show (Instruction s1 v1 s2 v2 d) = "(" ++ unwords [show s1, show v1, show s2, show v2, show d] ++ ")"
