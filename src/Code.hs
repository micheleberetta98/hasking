module Code
  ( MachineCode(..)
  , fromCode
  ) where

import           Data.Char         (isSpace)
import           Data.Either       (fromLeft, isLeft)
import           Data.List         (foldl')
import qualified Data.Map          as M
import           Error             (Error (..), ErrorType (..))
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

type WithErrors = Either [Error]

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
fromCode = build . map formatError . validateInstructions . parseInstructions . sanitize
  where
    parseInstructions = map (fmap parseInstruction)
    validateInstructions = map (fmap (>>= validate))
    formatError (i, Left e)  = Left [LineError i e]
    formatError (_, Right x) = Right x

-- | Removes the comments and the empty lines from the code, giving back only
-- the interesting bits
sanitize :: Code -> [WithLine String]
sanitize = filter notEmpty . map stripComment . addNumbers . lines
  where
    addNumbers = zip [1..]
    stripComment = fmap (dropWhile isSpace . takeWhile (/= ';'))
    notEmpty = not . null . snd

-- | Builds the `MachineCode` structure if all the instructions are correct, or it returns
-- a `Left ErrorList` with all the errors
build :: [WithErrors Instruction] -> WithErrors MachineCode
build ls =
  if null errs
    then sequence ls >>= buildMachine
    else Left $ foldl' (<>) mempty errs
  where
    errs = map (fromLeft mempty) $ filter isLeft ls

-- | Builds the `MachineCode` given a list of instructions and their lines
buildMachine :: [Instruction] -> WithErrors MachineCode
buildMachine = validateMachine . foldl' updateCode empty
  where
    validateMachine m =
      when noInputTape MissingInputTape m
        >>= when noInitialState NoInitialState
        >>= when noFinalStates NoFinalStates

    when fcheck err m
      | fcheck m       = Left [SimpleError err]
      | otherwise = Right m

    noInputTape = null . T.toList . initialTape
    noInitialState (MachineCode _ s _ _) = s == State ""
    noFinalStates (MachineCode _ _ fs _) = null fs

------------------------------------------------
-- Validations
------------------------------------------------

-- | It validates a single instruction, that could have been parsed correctly or not
validate :: Instruction -> Either ErrorType Instruction
validate (Control "BEGIN" [])    = Left NoInitialState
validate x@(Control "BEGIN" [_]) = Right x
validate (Control "BEGIN" _)     = Left MultiInitialState
validate (Control "FINAL" [])    = Left NoFinalStates
validate (TapeValue [])          = Left EmptyInputTape
validate x                       = Right x

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
