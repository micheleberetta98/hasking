module Code
  ( MachineCode(..)
  , parseCode
  ) where

import           Data.Char     (isSpace)
import           Data.List     (foldl')
import qualified Data.Map      as M
import           Error         (Error (..), ErrorList, ErrorType (..),
                                singleton)
import           Instruction   (Instruction (Control, Step, TapeValue),
                                parseInstruction)
import           Tape          (Direction, Symbol, Tape)
import qualified Tape          as T
import           TuringMachine (FromState, State (State), StateList, ToState,
                                Transitions)

------------------------------------------------
-- Data types
------------------------------------------------

-- | The raw lines that define the behaviour of the machine, each line defines an `Instruction`
type Code = String

-- | A line with its number
type LineCode = (Int, String)

-- | An instruction with its line number
type LineInstruction = (Int, Instruction)

type From = FromState String String
type To = ToState String String
type WithErrors = Either ErrorList

-- | The whole machine specification, derived from a piece of code
data MachineCode = MachineCode
  { transitions  :: Transitions String String
  , initialState :: State String
  , finalStates  :: StateList String
  , initialTape  :: Tape String
  } deriving (Show, Eq)

------------------------------------------------
-- Functions
------------------------------------------------

-- | An empty `MachineCode` structure
emptyC :: MachineCode
emptyC = MachineCode M.empty (State "") [] T.empty

-- | It converts the code into a `MachineCode` structure, with transitions, initial and final states
-- It returns `Nothing` if something goes wrong
parseCode :: Code -> WithErrors MachineCode
parseCode code = (buildCode . parseInstructions . stripComments $ lines' code) >>= mvalidate
  where
    buildCode = foldl' (+>) (Right emptyC)
    parseInstructions = map (format . fmap parseInstruction)

    format :: (Int, Either ErrorType Instruction) -> Either Error LineInstruction
    format (l, Left etype) = Left $ LineError l etype
    format (l, Right i)    = Right (l, i)

-- | Add line numbers to the code
lines' :: Code -> [LineCode]
lines' = zip [1..] . lines

-- | Removes a comment from a line
stripComments :: [LineCode] -> [LineCode]
stripComments = filter (not . isEmpty) . map (fmap stripComment)
  where
    stripComment = takeWhile (/= ';')
    isEmpty = null . dropWhile isSpace . snd

-- | Updates the machine code given a single instruction
updateCode :: MachineCode -> Instruction -> MachineCode
updateCode c (Control "BEGIN" [s]) = c{ initialState = s }
updateCode c (Control "FINAL" s)   = c{ finalStates = s }
updateCode c (TapeValue tape)      = c{ initialTape = T.fromList tape }
updateCode c s                     = addTransition (split s) c

-- | Inserts a new transitions into the existing ones
addTransition :: (From, To) -> MachineCode -> MachineCode
addTransition (from, to) c = c{ transitions = M.insert from to (transitions c) }

-- | It converts a `Step` instruction into a tuple made of `FromState` and `ToState`
split :: Instruction -> (From, To)
split (Step s1 v1 s2 v2 d) = (from, to)
  where
    from = (s1, v1)
    to = (s2, v2, d)

------------------------------------------------
-- Utils to combine errors
------------------------------------------------

-- | Combines an `WithErrors MachineCode` with a `Either Error LineInstruction`
(+>) :: WithErrors MachineCode -> Either Error LineInstruction -> WithErrors MachineCode
Left es +> Left e  = Left (es <> singleton e)
Left es +> Right i = Left es ++> ivalidate i
Right c +> Left e  = Left (singleton e)
Right c +> Right i = updateCode c <$> ivalidate i

-- | Combines two `WithErrors` together
(++>) :: WithErrors a -> WithErrors b -> WithErrors a
Left es1 ++> Left es2 = Left (es1 <> es2)
Right _ ++> Left es   = Left es
Left es ++> Right _   = Left es
Right x ++> Right _   = Right x

------------------------------------------------
-- Validations
------------------------------------------------

-- | Validates the final machine code
mvalidate :: MachineCode -> WithErrors MachineCode
mvalidate m@(MachineCode tr s ss t) =
  (trans tr ++> initial s ++> finals ss ++> tape t) >> Right m
  where
    trans t
      | t == M.empty = just $ SimpleError NoInstructions
      | otherwise    = Right ""
    initial (State "") = just $ SimpleError NoInitialState
    initial _          = Right ""
    finals [] = just $ SimpleError NoFinalStates
    finals _  = Right ""
    tape t
      | null (T.toList t) = just $ SimpleError MissingInputTape
      | otherwise         = Right ""

-- | Validates the instruction
ivalidate :: LineInstruction -> WithErrors Instruction
ivalidate (l, i) = ivalidate' i
  where
    ivalidate' (Control "BEGIN" [])     = just $ LineError l NoInitialState
    ivalidate' (Control "BEGIN" states)
      | length states > 1               = just $ LineError l MultiInitialState
    ivalidate' (Control "FINALS" [])    = just $ LineError l NoFinalStates
    ivalidate' (TapeValue [])           = just $ LineError l EmptyInputTape
    ivalidate' i                        = Right i

-- | Utility that creates a single `Left ErrorList`
just :: Error -> WithErrors b
just = Left . singleton
