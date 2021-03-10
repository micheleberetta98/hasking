module SExpression (parseInstruction) where

import           Control.Applicative (Alternative ((<|>)))
import           Parser              (Parser, char, direction, identifier,
                                      integer, spaces, spaces')
import           Tape                (Direction)

------------------------------------------------
-- Instructions and its types
------------------------------------------------

-- | An `Atom` is an identifier (string)
newtype Atom = Atom String deriving (Show)

-- | A `Value` is an integer (could be used as input/output)
data Value = IValue Int | SValue String deriving (Show)

-- | An expression for the turing machine is given by
-- - A state name (atom)
-- - Something to read (value)
-- - A new state name (atom)
-- - Something to write (value)
-- - A movement (Direction)
data Instruction = Instruction
  { fromState    :: Atom
  , valueRead    :: Value
  , toState      :: Atom
  , valueWritten :: Value
  , dir          :: Direction
  } deriving (Show)

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

    s1 = parseAtom
    s2 = parseAtom
    v1 = parseValue
    v2 = parseValue

    seps = spaces'

-- | Parses an atom
parseAtom :: Parser Atom
parseAtom = Atom <$> identifier

-- | Parses a value
parseValue :: Parser Value
parseValue = parseSValue <|> parseIValue
  where
    parseSValue = SValue <$> identifier
    parseIValue = IValue <$> integer

------------------------------------------------
-- Building a Map for all the instructions (TODO)
------------------------------------------------
