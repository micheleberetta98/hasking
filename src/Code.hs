module Code
  ( MachineCode(..)
  , parseCode
  ) where

import           Data.List
import qualified Data.Map      as M
import           Instruction   (Instruction (Control, Step, TapeValue),
                                parseInstruction)
import           Tape          (Tape)
import qualified Tape          as T
import           TuringMachine (FromState, State (State), StateList, ToState,
                                Transitions)

------------------------------------------------
-- Data types
------------------------------------------------

-- | The raw lines that define the behaviour of the machine, each line defines an `Instruction`
type Code = String

type From = FromState String String
type To = ToState String String
type WithError = Either String

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
empty :: MachineCode
empty = MachineCode M.empty (State "") [] T.empty

-- | It converts the code into a `MachineCode` structure, with transitions, initial and final states
-- It returns `Nothing` if something goes wrong
parseCode :: Code -> WithError MachineCode
parseCode code = (buildCode . map stripComment $ lines code) >>= validate
  where
    buildCode = foldl' addLine (Right empty)

    addLine c ""   = c
    addLine c line = updateCode (parseInstruction line) c

-- | Removes a comment from a line
stripComment :: String -> String
stripComment = takeWhile (/= '#')

-- | Validates the final machine code
validate :: MachineCode -> WithError MachineCode
validate (MachineCode t _ _ _)
  | t == M.empty                        = Left "No instructions provided"
validate (MachineCode _ (State "") _ _) = Left "No initial state provided"
validate (MachineCode _ _ [] _)         = Left "No final states provided"
validate (MachineCode _ _ _ t)
  | null (T.toList t)                   = Left "No tape provided"
validate m                              = Right m

-- | Updates the machine code given a single instruction
updateCode :: WithError Instruction -> WithError MachineCode -> WithError MachineCode
updateCode (Right s@Step {})             = addTransition $ split s
updateCode (Right c@(Control "BEGIN" s)) = setInitialState s
updateCode (Right c@(Control "FINAL" s)) = setFinalStates s
updateCode (Right t@(TapeValue tape))    = withTape tape
updateCode (Left s)                      = const (Left s)

-- | Inserts a new transitions into the existing ones
addTransition :: (From, To) -> WithError MachineCode -> WithError MachineCode
addTransition (from, to) (Right c) = Right $ c{ transitions = M.insert from to (transitions c) }
addTransition _  l                 = l

-- | Updates the initial state of the machine code
setInitialState :: [State String] -> WithError MachineCode -> WithError MachineCode
setInitialState [state] (Right c) = Right $ c{ initialState = state }
setInitialState [] (Right _)      = Left "No initial state provided"
setInitialState states (Right _)  = Left "More than one initial state"
setInitialState _ l               = l

-- | Updates the final states of the machine code
setFinalStates :: StateList String -> WithError MachineCode -> WithError MachineCode
setFinalStates [] _             = Left "No final states provided"
setFinalStates states (Right c) = Right $ c{ finalStates = states }
setFinalStates _ l              = l

-- | Updates the initial tape value
withTape :: [String] -> WithError MachineCode -> WithError MachineCode
withTape [] (Right _)   = Left "Empty input tape provided"
withTape tape (Right c) = Right $ c{ initialTape = T.fromList tape }
withTape _ l            = l

-- | It converts a `Step` instruction into a tuple made of `FromState` and `ToState`
split :: Instruction -> (From, To)
split (Step s1 v1 s2 v2 d) = (from, to)
  where
    from = (s1, v1)
    to = (s2, v2, d)
