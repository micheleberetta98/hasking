module Code
  ( MachineCode(..)
  , fromCode
  ) where

import           Data.Bifunctor    (Bifunctor (first))
import           Data.Char         (isSpace)
import           Data.Either       (fromLeft, isLeft)
import           Data.List         (foldl', partition)
import qualified Data.Map          as M
import           Error             (Error (..), ErrorList, ErrorType (..),
                                    singleton)
import           InstructionParser (Instruction (..), parseInstruction)
import           Tape              (Tape)
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

-- | An empty `MachineCode` structure
empty :: MachineCode
empty = MachineCode M.empty (State "") [] T.empty

-- | It converts the code into a `MachineCode` structure, with transitions, initial and final states
-- It returns `Left ErrorList` if something goes wrong
fromCode :: Code -> WithErrors MachineCode
fromCode = build . map (fmap validate) . parse . sanitize
  where parse = map (fmap parseInstruction)

-- | Removes the comments and the empty lines from the code, giving back only
-- the interesting bits
sanitize :: Code -> [WithLine String]
sanitize = filter notEmpty . map stripComment . addLineNumbers
  where
    addLineNumbers = zip [1..] . lines
    stripComment = fmap (dropWhile isSpace . takeWhile (/= ';'))
    notEmpty = not . null . snd

-- | Builds the `MachineCode` structure if all the instructions are correct, or it returns
-- a `Left ErrorList` with all the errors
build :: [WithLine (WithError Instruction)] -> WithErrors MachineCode
build ls =
  if null errs
    then sequence instructions >>= buildMachine
    else Left . foldl' (<>) mempty $ map (fromLeft mempty) errs
  where
    (errs, instructions) = partition isLeft $ map formatErrors ls
    formatErrors (l, x) = first (singleton . LineError l) x

-- | Builds the `MachineCode` given a list of instructions and their lines
buildMachine :: [Instruction] -> WithErrors MachineCode
buildMachine = validateMachine . foldl' updateCode empty
  where
    validateMachine m
      | noInputTape m = Left . singleton $ SimpleError MissingInputTape
      | noInitialState m = Left . singleton $ SimpleError NoInitialState
      | noFinalStates m = Left . singleton $ SimpleError NoFinalStates
      | otherwise = Right m

    noInputTape = null . T.toList . initialTape
    noInitialState (MachineCode _ s _ _) = s == State ""
    noFinalStates (MachineCode _ _ fs _) = null fs

------------------------------------------------
-- Validations
------------------------------------------------

-- | It validates a single instruction, that could have been parsed correctly or not
validate :: Either ErrorType Instruction -> Either ErrorType Instruction
validate i = i >>= validate'
  where
    validate' (Control "BEGIN" [])    = Left NoInitialState
    validate' x@(Control "BEGIN" [_]) = Right x
    validate' (Control "BEGIN" _)     = Left MultiInitialState
    validate' (Control "FINAL" [])    = Left NoFinalStates
    validate' (TapeValue [])          = Left EmptyInputTape
    validate' x                       = Right x

------------------------------------------------
-- Utilities
------------------------------------------------

-- | Updates the machine code given a single instruction
updateCode :: MachineCode -> Instruction -> MachineCode
updateCode c (Control "BEGIN" s)  = c{ initialState = head s }
updateCode c (Control "FINAL" s)  = c{ finalStates = s }
updateCode c (TapeValue tape)     = c{ initialTape = T.fromList tape }
updateCode c (Step s1 v1 s2 v2 d) = addTransition ((s1, v1), (s2, v2, d)) c
updateCode _ (Control x _)        = error $ "This shouldn't have happened, found invalid control with name `" ++ x ++ "`"

-- | Inserts a new transitions into the existing ones
addTransition :: (From String String, To String String) -> MachineCode -> MachineCode
addTransition (from, to) c = c{ transitions = M.insert from to (transitions c) }
