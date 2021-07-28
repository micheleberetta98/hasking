module Code
  ( MachineCode(..)
  , fromCode
  ) where

import           Control.Applicative (Applicative (liftA2))
import           Data.Char           (isSpace)
import           Data.Either         (fromLeft, isLeft)
import           Data.List           (foldl')
import qualified Data.Map            as M
import           Error               (Error (..), ErrorType (..))
import           Instruction         (Command (..), Instruction (..),
                                      parseInstruction, validate)
import           Tape                (Tape)
import qualified Tape                as T
import           TuringMachine       (From, State (State), To, Transitions)
import           Validation          (Validation (..), whenIsOk)

------------------------------------------------
-- Data types
------------------------------------------------

-- | The raw lines that define the behaviour of the machine, each line defines an `Instruction`
type Code = String

-- | A type with its line number
type WithLine a = (Int, a)

type WithErrors = Validation [Error]

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
    formatError (i, Left e)  = Err [LineError i e]
    formatError (_, Right x) = Ok x

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
build = whenIsOk validateMachine . foldl' (liftA2 updateCode) (Ok empty)

-- | Validates the entire `MachineCode`
validateMachine :: MachineCode -> WithErrors MachineCode
validateMachine (MachineCode trans initial finals tape) =
  MachineCode
    <$> Ok trans
    <*> checkInitialState initial
    <*> checkFinalStates finals
    <*> checkInputTape tape
  where
    checkInputTape = when (null . T.toList) MissingInputTape
    checkInitialState = when (== State "") NoInitialState
    checkFinalStates = when null NoFinalStates

    when f err x
      | f x        = Err [SimpleError err]
      | otherwise  = Ok x

------------------------------------------------
-- Utilities
------------------------------------------------

-- | Updates the machine code given a single instruction
updateCode :: MachineCode -> Instruction -> MachineCode
updateCode c (Control Begin s)    = c{ initialState = head s }
updateCode c (Control Final s)    = c{ finalStates = s }
updateCode c (TapeValue tape)     = c{ initialTape = T.fromList tape }
updateCode c (Step s1 v1 s2 v2 d) = addTransition ((s1, v1), (s2, v2, d)) c

-- | Inserts a new transitions into the existing ones
addTransition :: (From String String, To String String) -> MachineCode -> MachineCode
addTransition (from, to) c = c{ transitions = M.insert from to (transitions c) }
