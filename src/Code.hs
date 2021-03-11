module Code
  ( MachineCode(..)
  , parseCode
  ) where

import           Data.List     (foldl')
import qualified Data.Map      as M
import           Instruction
import           Parser
import           Tape          (Tape, fromList)
import qualified Tape          as T
import           TuringMachine

------------------------------------------------
-- Data types
------------------------------------------------

-- | The raw lines that define the behaviour of the machine, each line defines an `Instruction`
type Code = String

data MachineCode a = MachineCode
  { transitions  :: Transitions a
  , initialState :: State
  , finalStates  :: StateList
  , initialTape  :: Tape String
  } deriving (Show, Eq)

------------------------------------------------
-- Functions
------------------------------------------------

-- | An empty `MachineCode` structure
empty :: (Ord a) => MachineCode a
empty = MachineCode M.empty (State "") [] T.empty

-- | It converts the code into a `MachineCode` structure, with transitions, initial and final states
-- It returns `Nothing` if something goes wrong
parseCode :: Code -> Maybe (MachineCode String)
parseCode = foldl' f (Just empty) . lines
  where
    f code line = updateCode code (parseInstruction line)

updateCode :: Maybe (MachineCode String) -> Maybe Instruction -> Maybe (MachineCode String)
updateCode Nothing _          = Nothing
updateCode _ Nothing          = Nothing
updateCode (Just c) (Just s@Step {}) =
  let (from, to) = convertStep s
  in Just $ c{ transitions = M.insert from to (transitions c) }
updateCode (Just c) (Just s@(Control "BEGIN" state)) =
  Just $ c{ initialState = head state }
updateCode (Just c) (Just s@(Control "FINAL" states)) =
  Just $ c{ finalStates = states }
updateCode (Just c) (Just s@(TapeValue tape)) =
  Just $ c{ initialTape = fromList tape }

-- | It converts a `Step` instruction into a tuple made of `FromState` and `ToState`
convertStep :: Instruction -> (FromState String, ToState String)
convertStep (Step s1 v1 s2 v2 d) = (from, to)
  where
    from = (s1, valueSymbol v1)
    to = (s2, valueSymbol v2, d)
