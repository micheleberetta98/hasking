module InstructionParser
  ( Instruction(..)
  , parseInstruction
  ) where

import           Control.Applicative (Alternative ((<|>)))
import           Data.Functor        (($>))
import           Error               (ErrorType (InvalidInstruction, UnrecognizedChars))
import           Parser              (Parser (..), alpha, alphaNum, anyOf, char,
                                      identifier, spaced, zeroOrMore)
import           Pretty              (Pretty (..), prettyList, wrap)
import           Tape                (Direction (..), Symbol (..))
import           TuringMachine       (State (..))

------------------------------------------------
-- Instructions and its types
------------------------------------------------

-- | An instruction for the turing machine can be
-- - A `Step` which has
--   - A state name (state id)
--   - Something to read (value)
--   - A new state name (state id)
--   - Something to write (value)
--   - A movement (`Direction`)
-- - A `Control`, which has a command and a list of states
-- - A `TapeValue`, the input tape
data Instruction =
  Step
    { fromState    :: State String
    , valueRead    :: Symbol String
    , toState      :: State String
    , valueWritten :: Symbol String
    , dir          :: Direction
    }
  | Control
    { command :: String
    , value   :: [State String]
    }
  | TapeValue [Symbol String]
  deriving (Show, Eq)

------------------------------------------------
-- Parsing a single instruction
------------------------------------------------

-- | Parses an instruction in the form  of a step such as `(fromState valueRead toState valueWritten dir)`
-- or in the form of a control such as `[NAME s1 s2 s3 ...]`
-- parseInstruction :: String -> Instruction
parseInstruction :: String -> Either ErrorType Instruction
parseInstruction s =
  case runParser (step <|> control <|> tape) s of
    Just (i, "") -> Right i
    Just (_, _)  -> Left UnrecognizedChars
    _            -> Left InvalidInstruction

-- | Parses a step in the such as `(s1 v1 s2 v2 dir)`
step :: Parser Instruction
step = delimiter '(' *> s <* delimiter ')'
  where
    s = Step
      <$> spaced state
      <*> spaced symbol
      <*> spaced state
      <*> spaced symbol
      <*> spaced direction

-- | Parses a control sequence in the form `[NAME s1 s2 s3 ...]`
control :: Parser Instruction
control = delimiter '[' *> c <* delimiter ']'
  where
    c = Control <$> spaced alpha <*> zeroOrMore (spaced state)

-- | Parses the initial value of the tape, namely `{Symbol Symbol ...}`
tape :: Parser Instruction
tape = delimiter '{' *> t <* delimiter '}'
  where t = TapeValue <$> zeroOrMore (spaced symbol)

-- | Parses a state value
state :: Parser (State String)
state = State <$> identifier

-- | Parses a symbol
symbol :: Parser (Symbol String)
symbol = blank <|> (Symbol <$> symbolValue)
  where
    blank = char '.' $> Blank
    symbolValue = alphaNum <|> anyOf "*#"

-- | Parses a direction
direction :: Parser Direction
direction = toDirection <$> (char 'L' <|> char 'R' <|> char 'S')
  where
    toDirection 'L' = L
    toDirection 'R' = R
    toDirection 'S' = S
    toDirection _   = S -- Fallback, should never happen

-- | Parses a delimiter - a start or a stop sequence
delimiter :: Char -> Parser Char
delimiter = spaced . char

------------------------------------------------
-- Instances
------------------------------------------------

instance Pretty Instruction where
  pretty (Step s1 v1 s2 v2 d) = wrap "(" (prettyList [s1', v1', s2', v2', pretty d]) ")"
    where
      s1' = pretty s1
      v1' = pretty v1
      s2' = pretty s2
      v2' = pretty v2
  pretty (Control n v) = wrap "[" (prettyList (n : map pretty v)) "]"
  pretty (TapeValue t)     = wrap "{" (prettyList t) "}"

