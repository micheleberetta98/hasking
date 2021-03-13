module Code
  ( MachineCode(..)
  , parseCode
  ) where

import           Data.List
import qualified Data.Map      as M
import           Instruction   (Instruction (Control, Step, TapeValue),
                                parseInstruction)
import           LineError     (ErrorList, justOne, simpleError, (.+))
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
type WithError = Either ErrorList

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
parseCode code = (buildCode . map parseInstruction . stripComments $ lines code) >>= validate
  where
    buildCode :: [Either String Instruction] -> WithError MachineCode
    buildCode = foldl' combine (Right empty)

    combine (Left es) (Left l) = Left $ es .+ simpleError l
    combine (Left es) _        = Left es
    combine (Right c) l        = updateCode l c

-- | Removes a comment from a line
stripComments :: [String] -> [String]
stripComments = filter (/= "") . map (takeWhile (/= '#'))

-- | Validates the final machine code
validate :: MachineCode -> WithError MachineCode
validate (MachineCode t _ _ _)
  | t == M.empty                        = Left. justOne $ simpleError "No instructions provided"
validate (MachineCode _ (State "") _ _) = Left. justOne $ simpleError "No initial state provided"
validate (MachineCode _ _ [] _)         = Left. justOne $ simpleError "No final states provided"
validate (MachineCode _ _ _ t)
  | null (T.toList t)                   = Left . justOne $ simpleError "No tape provided"
validate m                              = Right m

-- | Updates the machine code given a single instruction
updateCode :: Either String Instruction -> MachineCode -> WithError MachineCode
updateCode (Right s@Step {})             = addTransition $ split s
updateCode (Right c@(Control "BEGIN" s)) = setInitialState s
updateCode (Right c@(Control "FINAL" s)) = setFinalStates s
updateCode (Right t@(TapeValue tape))    = withTape tape
updateCode (Left s)                      = const . Left .justOne $ simpleError s

-- | Inserts a new transitions into the existing ones
addTransition :: (From, To) -> MachineCode -> WithError MachineCode
addTransition (from, to) c = Right $ c{ transitions = M.insert from to (transitions c) }

-- | Updates the initial state of the machine code
setInitialState :: [State String] -> MachineCode -> WithError MachineCode
setInitialState [state] c = Right $ c{ initialState = state }
setInitialState []  _     = Left . justOne $ simpleError "No initial state provided"
setInitialState states  _ = Left . justOne $ simpleError "More than one initial state"

-- | Updates the final states of the machine code
setFinalStates :: StateList String -> MachineCode -> WithError MachineCode
setFinalStates [] _     = Left . justOne $ simpleError "No final states provided"
setFinalStates states c = Right $ c{ finalStates = states }

-- | Updates the initial tape value
withTape :: [String] -> MachineCode -> WithError MachineCode
withTape [] _   = Left . justOne $ simpleError "Empty input tape provided"
withTape tape c = Right $ c{ initialTape = T.fromList tape }

-- | It converts a `Step` instruction into a tuple made of `FromState` and `ToState`
split :: Instruction -> (From, To)
split (Step s1 v1 s2 v2 d) = (from, to)
  where
    from = (s1, v1)
    to = (s2, v2, d)
