module Instruction
  ( Instruction(..)
  , parseInstruction
  ) where

import           Control.Applicative (Alternative ((<|>)))
import           Data.Functor        (($>))
import           Parser              (Parser (..), astring, atom, char,
                                      identifier, spaced, spaced1, zeroOrMore)
import           Tape                (Direction (..), Symbol (..))
import           TuringMachine       (State (..))

------------------------------------------------
-- Instructions and its types
------------------------------------------------

-- | An argument for an instruction: can be either a `State String`, or a `Symbol String` or a `Direction`
data Arg = AState (State String) | ASymbol (Symbol String) | ADir Direction

-- | An instruction for the turing machine can be
-- - A `Step` which has
--  - A state name (state id)
--  - Something to read (value)
--  - A new state name (state id)
--  - Something to write (value)
--  - A movement (Direction)
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
  | TapeValue [String]
  deriving (Eq)

------------------------------------------------
-- Getting values
------------------------------------------------

argState :: Arg -> State String
argState (AState s) = s

argSymbol :: Arg -> Symbol String
argSymbol (ASymbol s) = s

argDir :: Arg -> Direction
argDir (ADir d) = d

------------------------------------------------
-- Parsing a single instruction
------------------------------------------------

-- | Parses an instruction in the form  of a step such as `(fromState valueRead toState valueWritten dir)`
-- or in the form of a control such as `[NAME s1 s2 s3 ...]`
-- parseInstruction :: String -> Instruction
parseInstruction :: String -> Either String Instruction
parseInstruction s =
  case runParser (step <|> control <|> tape) s of
    Just (i, "") -> Right i
    Just (i, _)  -> Left "Unrecognized characters after the instruction"
    _            -> Left "Invalid instruction"

-- | Parses a step in the such as `(s1 v1 s2 v2 dir)`
step :: Parser Instruction
step = delimiter '(' *> s <* delimiter ')'
  where
    s = Step
      <$> spaced1 state
      <*> spaced1 symbol
      <*> spaced1 state
      <*> spaced1 symbol
      <*> spaced direction

-- | Parses a control sequence in the form `[NAME s1 s2 s3 ...]`
control :: Parser Instruction
control = delimiter '[' *> c <* delimiter ']'
  where
    c = Control <$> spaced1 astring <*> values

    values = (:) <$> spaced state <*> zeroOrMore (spaced state)

-- | Parses the initial value of the tape, namely `{Symbol Symbol ...}`
tape :: Parser Instruction
tape = delimiter '{' *> t <* delimiter '}'
  where t = TapeValue <$> zeroOrMore (spaced atom)

-- | Parses a state value
state :: Parser (State String)
state = State <$> identifier

-- | Parses a value
symbol :: Parser (Symbol String)
symbol = (Symbol <$> atom) <|> blank
  where blank = char '.' $> Blank

direction :: Parser Direction
direction = toDirection <$> (char 'L' <|> char 'R' <|> char 'S')
  where
    toDirection 'L' = L
    toDirection 'R' = R
    toDirection 'S' = S

-- | Parses a delimiter - a start or a stop sequence
delimiter :: Char -> Parser Char
delimiter = spaced . char

------------------------------------------------
-- Instances
------------------------------------------------

instance Show Instruction where
  show (Step s1 v1 s2 v2 d) = wrapped '(' [s1', v1', s2', v2', show d] ')'
    where
      s1' = getState s1
      v1' = prettySymbol v1
      s2' = getState s2
      v2' = prettySymbol v2

  show (Control name value) = wrapped '[' (name : map getState value) ']'
  show (TapeValue tape)     = wrapped '{' tape '}'

prettySymbol :: Symbol String -> String
prettySymbol (Symbol s) = s
prettySymbol Blank      = "."

wrapped :: Char -> [String] -> Char -> String
wrapped l content r = [l] ++ unwords content ++ [r]
