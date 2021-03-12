module Code
  ( MachineCode(..)
  , parseCode
  ) where

import           Data.List     (foldl')
import qualified Data.Map      as M
import           Instruction   (Instruction (Control, Step, TapeValue),
                                parseInstruction, stateId, valueSymbol)
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
parseCode :: Code -> Maybe MachineCode
parseCode = foldl' f (Just empty) . lines
  where
    f code line = updateCode (parseInstruction line) code

-- | Updates the machine code given a single instruction
updateCode :: Maybe Instruction -> Maybe MachineCode -> Maybe MachineCode
updateCode (Just s@Step {})             = withTransition $ convertStep s
updateCode (Just c@(Control "BEGIN" s)) = withInitialState $ map stateId s
updateCode (Just c@(Control "FINAL" s)) = withFinalStates $ map stateId s
updateCode (Just t@(TapeValue tape))    = withTape tape
updateCode _                            = const Nothing

-- | Inserts a new transitions into the existing ones
withTransition :: (From, To) -> Maybe MachineCode -> Maybe MachineCode
withTransition (from, to) (Just c) = Just $ c{ transitions = M.insert from to (transitions c) }
withTransition _  _                = Nothing

-- | Updates the initial state of the machine code
withInitialState :: [State String] -> Maybe MachineCode -> Maybe MachineCode
withInitialState [state] (Just c) = Just $ c{ initialState = state }
withInitialState _ _              = Nothing

-- | Updates the final states of the machine code
withFinalStates :: StateList String -> Maybe MachineCode -> Maybe MachineCode
withFinalStates states (Just c) = Just $ c{ finalStates = states }
withFinalStates _ _             = Nothing

-- | Updates the initial tape value
withTape :: [String] -> Maybe MachineCode -> Maybe MachineCode
withTape tape (Just c) = Just $ c{ initialTape = T.fromList tape }
withTape _ _           = Nothing

-- | It converts a `Step` instruction into a tuple made of `FromState` and `ToState`
convertStep :: Instruction -> (From, To)
convertStep (Step s1 v1 s2 v2 d) = (from, to)
  where
    from = (stateId s1, valueSymbol v1)
    to = (stateId s2, valueSymbol v2, d)
