module Code
  ( MachineCode(..)
  , parseCode
  ) where

import           Data.List     (foldl')
import qualified Data.Map      as M
import           Instruction   (Instruction (Control, Step, TapeValue),
                                parseInstruction, stateId, valueSymbol)
import           Parser
import           Tape          (Tape)
import qualified Tape          as T
import           TuringMachine (FromState, State (State), StateList, ToState,
                                Transitions)

------------------------------------------------
-- Data types
------------------------------------------------

-- | The raw lines that define the behaviour of the machine, each line defines an `Instruction`
type Code = String

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
    f code line = updateCode code (parseInstruction line)

updateCode :: Maybe MachineCode -> Maybe Instruction -> Maybe MachineCode
updateCode Nothing _          = Nothing
updateCode _ Nothing          = Nothing
updateCode (Just c) (Just s@Step {}) =
  let (from, to) = convertStep s
  in Just $ c{ transitions = M.insert from to (transitions c) }
updateCode (Just c) (Just s@(Control "BEGIN" state)) =
  Just $ c{ initialState = stateId $ head state }
updateCode (Just c) (Just s@(Control "FINAL" states)) =
  Just $ c{ finalStates = map stateId states }
updateCode (Just c) (Just s@(TapeValue tape)) =
  Just $ c{ initialTape = T.fromList tape }

-- | It converts a `Step` instruction into a tuple made of `FromState` and `ToState`
convertStep :: Instruction -> (FromState String String, ToState String String)
convertStep (Step s1 v1 s2 v2 d) = (from, to)
  where
    from = (stateId s1, valueSymbol v1)
    to = (stateId s2, valueSymbol v2, d)
