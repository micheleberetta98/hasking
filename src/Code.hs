module Code
  ( MachineCode(..)
  , parseCode
  ) where

import           Data.Bifunctor    (Bifunctor (bimap))
import           Data.Char         (isSpace)
import           Data.Either       (isLeft)
import           Data.List         (foldl', partition)
import qualified Data.Map          as M
import           Error             (Error (..), ErrorList, ErrorType (..),
                                    fromList, singleton)
import           InstructionParser (Instruction (..), parseInstruction)
import           Tape              (Direction, Symbol, Tape)
import qualified Tape              as T
import           TuringMachine     (From, State (State), To, Transitions)

------------------------------------------------
-- Data types
------------------------------------------------

-- | The raw lines that define the behaviour of the machine, each line defines an `Instruction`
type Code = String

-- | A type with its line number
type WithLine a = (Int, a)

type WithError = Either ErrorType
type WithErrors = Either ErrorList

-- | The whole machine specification, derived from a piece of code
data MachineCode = MachineCode
  { transitions  :: Transitions String String
  , initialState :: State String
  , finalStates  :: [State String]
  , initialTape  :: Tape String
  } deriving (Show, Eq)

------------------------------------------------
-- Functions for code parsing
------------------------------------------------

-- -- | An empty `MachineCode` structure
-- empty :: MachineCode
-- empty = MachineCode M.empty (State "") [] T.empty

-- | It converts the code into a `MachineCode` structure, with transitions, initial and final states
-- It returns `Left ErrorList` if something goes wrong
parseCode :: Code -> WithErrors MachineCode
parseCode = buildCode . map (fmap parseInstruction) . sanitizeCode

-- | Removes the comments and the empty lines from the code, giving back only
-- the interesting bits
sanitizeCode :: Code -> [WithLine String]
sanitizeCode = filter (not . isEmpty) . map stripComment . addLineNumbers
  where
    addLineNumbers = zip [1..] . lines
    stripComment = fmap (dropWhile isSpace . takeWhile (/= ';'))
    isEmpty = null . snd

-- | Builds the `MachineCode` structure if all the instructions are correct, or it returns
-- a `Left ErrorList` with all the errors
buildCode :: [WithLine (WithError Instruction)] -> WithErrors MachineCode
buildCode ls =
  let (errs, instructions) = splitErrors ls
  in if null errs
    then buildMachine instructions
    else Left $ fromList errs

-- | Used to separate all the errors and instructions from
splitErrors :: [WithLine (WithError Instruction)] -> ([Error], [Instruction])
splitErrors = bimap toErrors toInstructions . partition isLeft . map (addLine . fmap validate)
  where
    addLine (_, Right x)  = Right x
    addLine (l, Left err) = Left $ LineError l err

    toErrors = map (\(Left x) -> x)
    toInstructions = map (\(Right x) -> x)

-- | Builds the `MachineCode` given a list of instructions and their lines
buildMachine :: [Instruction] -> WithErrors MachineCode
buildMachine = validateInputTape . foldl' updateCode empty
  where
    empty = MachineCode M.empty (State "") [] T.empty
    validateInputTape m
      | hasInputTape m = Right m
      | otherwise = Left . singleton $ SimpleError MissingInputTape

------------------------------------------------
-- Validations
------------------------------------------------

-- | It validates a single instruction, that could have been parsed correctly or not
validate :: Either ErrorType Instruction -> Either ErrorType Instruction
validate i = i >>= validate'
  where
    validate' (Control "BEGIN" [])    = Left NoInitialState
    validate' i@(Control "BEGIN" [s]) = Right i
    validate' (Control "BEGIN" ss)    = Left MultiInitialState
    validate' (Control "FINALS" [])   = Left NoFinalStates
    validate' (TapeValue [])          = Left EmptyInputTape
    validate' i                       = Right i

-- | Checks wether the initial tape is present or not
hasInputTape :: MachineCode -> Bool
hasInputTape = not . null . T.toList . initialTape

------------------------------------------------
-- Utilities
------------------------------------------------

-- | Updates the machine code given a single instruction
updateCode :: MachineCode -> Instruction -> MachineCode
updateCode c (Control "BEGIN" [s]) = c{ initialState = s }
updateCode c (Control "FINAL" s)   = c{ finalStates = s }
updateCode c (TapeValue tape)      = c{ initialTape = T.fromList tape }
updateCode c s                     = addTransition (split s) c

-- | Inserts a new transitions into the existing ones
addTransition :: (From String String, To String String) -> MachineCode -> MachineCode
addTransition (from, to) c = c{ transitions = M.insert from to (transitions c) }

-- | It converts a `Step` instruction into a tuple made of `From` and `To`
split :: Instruction -> (From String String, To String String)
split (Step s1 v1 s2 v2 d) = (from, to)
  where
    from = (s1, v1)
    to = (s2, v2, d)
