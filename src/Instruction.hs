module Instruction
  ( Instruction(..)
  , parseInstruction
  , stateId
  , valueSymbol
  ) where

import           Control.Applicative (Alternative ((<|>)))
import           Data.Functor        (($>))
import           Parser              (Parser (runParser), alphaString, atom,
                                      char, identifier, integer, oneOrMore,
                                      spaces, spaces1, zeroOrMore)
import           Tape                (Direction (..), Symbol (..), Tape,
                                      fromList, toList)
import           TuringMachine       (State (State))

------------------------------------------------
-- Instructions and its types
------------------------------------------------

-- | An `IState` is an instruction-friendly state name
newtype IState = IState (State String)

-- | A `StateList` of string states
type IStateList = [IState]

-- | A `ISymbol` is an instruction-friendly input/output on the tape (integer or string)
newtype ISymbol = ISymbol (Symbol String)

-- | An expression for the turing machine is given by
-- - A state name (state id)
-- - Something to read (value)
-- - A new state name (state id)
-- - Something to write (value)
-- - A movement (Direction)
data Instruction =
  Step
    { fromState    :: IState
    , valueRead    :: ISymbol
    , toState      :: IState
    , valueWritten :: ISymbol
    , dir          :: Direction
    }
  | Control
    { command :: String
    , value   :: IStateList
    }
  | TapeValue [String]
  deriving (Eq)

------------------------------------------------
-- Getting values
------------------------------------------------

stateId :: IState -> State String
stateId (IState s) = s

valueSymbol :: ISymbol -> Symbol String
valueSymbol (ISymbol s) = s

------------------------------------------------
-- Parsing a single instruction
------------------------------------------------

-- | Parses an instruction in the form  of a step such as `(fromState valueRead toState valueWritten dir)`
-- or in the form of a control such as `[NAME s1 s2 s3 ...]`
-- parseInstruction :: String -> Instruction
parseInstruction :: String -> Either String Instruction
parseInstruction s =
  case runParser (parseStep <|> parseControl <|> parseTape) s of
    Just (i, "") -> Right i
    Just (i, _)  -> Left "Unrecognized characters after the instruction"
    _            -> Left "Invalid instruction"

-- | Parses a step in the such as `(s1 v1 s2 v2 dir)`
parseStep :: Parser Instruction
parseStep = delimiter '(' *> instruction <* delimiter ')'
  where
    instruction = Step
      <$> parseState <* spaces1
      <*> parseSymbol <* spaces1
      <*> parseState <* spaces1
      <*> parseSymbol <* spaces1
      <*> parseDirection

-- | Parses a control sequence in the form `[NAME s1 s2 s3 ...]`
parseControl :: Parser Instruction
parseControl = delimiter '[' *> control <* delimiter ']'
  where
    control = Control
      <$> alphaString <* spaces1
      <*> values

    values = (:) <$> stateWithSpaces <*> zeroOrMore stateWithSpaces
    stateWithSpaces = spaces *> parseState <* spaces

-- | Parses the initial value of the tape, namely `{Symbol Symbol ...}`
parseTape :: Parser Instruction
parseTape = delimiter '{' *> tape <* delimiter '}'
  where tape = TapeValue <$> oneOrMore (spaces *> atom <* spaces)

-- | Parses a state value
parseState :: Parser IState
parseState = IState . State <$> identifier

-- | Parses a value
parseSymbol :: Parser ISymbol
parseSymbol = ISymbol <$> parseSymbol
  where
    parseSymbol = parseBlank <|> (Symbol <$> atom)
    parseBlank = char '.' $> Blank

parseDirection :: Parser Direction
parseDirection = toDirection <$> (char 'L' <|> char 'R' <|> char 'S')
  where
    toDirection 'L' = L
    toDirection 'R' = R
    toDirection 'S' = S

-- | Parses a delimiter - a start or a stop sequence
delimiter :: Char -> Parser Char
delimiter c = spaces *> char c <* spaces

------------------------------------------------
-- Instances
------------------------------------------------

instance Eq IState where
  (IState a) == (IState b) = a == b

instance Eq ISymbol where
  (ISymbol a) == (ISymbol b) = a == b

instance Show IState where
  show (IState (State s)) = s

instance Show ISymbol where
  show (ISymbol (Symbol i)) = i
  show (ISymbol Blank)      = "."

instance Show Instruction where
  show (Step s1 v1 s2 v2 d) = wrapped "(" [show s1, show v1, show s2, show v2, show d] ")"
  show (Control name value) = wrapped "[" (name : map show value) "]"
  show (TapeValue tape)     = wrapped "{" tape "}"

wrapped l content r = l ++ unwords content ++ r
