module Code.Internal where

import           Control.Monad.State
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Pretty
import           Tape                (Tape)
import           TuringMachine       (TuringMachine)
import qualified TuringMachine       as TM

-----------------------------------------------
-- Types
-----------------------------------------------

type Code = [Expression]

data Expression
  = Definition { defName :: MachineName, defMachine :: TuringMachine String String }
  | Simulation { simName :: MachineName, simTape :: Tape String }
  deriving (Show, Eq)

type MachineName = String

type Env = Map MachineName (TuringMachine String String)

data CodeOutput
  = Empty
  | TapeResult MachineName (Tape String) (Tape String)
  | MachineNotDefined MachineName
  | InvalidState MachineName (TM.From String String)
  deriving (Show, Eq)

-----------------------------------------------
-- Execution
-----------------------------------------------

execute :: Code -> [CodeOutput]
execute = filter (/= Empty) . flip evalState M.empty . mapM executeExpression

executeExpression :: Expression -> State Env CodeOutput
executeExpression (Definition name machine) = modify' (M.insert name machine) >> pure Empty
executeExpression (Simulation name tape) = gets (executeMachine name tape . M.lookup name)

executeMachine :: MachineName
                  -> Tape String
                  -> Maybe (TuringMachine String String)
                  -> CodeOutput
executeMachine name _ Nothing = MachineNotDefined name
executeMachine name tape (Just m) = toCodeOutput (TM.runMachine m tape)
  where
    toCodeOutput (Left s)           = InvalidState name s
    toCodeOutput (Right (_, tape')) = TapeResult name tape tape'

-----------------------------------------------
-- Utilities
-----------------------------------------------

getDef :: MachineName -> Code -> Maybe (TuringMachine String String)
getDef name code = defMachine <$> find (isDefinitionOf name) code

isDefinitionOf :: MachineName -> Expression -> Bool
isDefinitionOf name (Definition machine _) = name == machine
isDefinitionOf _ _                         = False

-----------------------------------------------
-- Instances
-----------------------------------------------

instance Pretty CodeOutput where
  pretty Empty = ""
  pretty (MachineNotDefined name) = "! Not defined: " <> name
  pretty (InvalidState name s) = "! " <> name <> " reached an invalid state: " <> pretty s
  pretty (TapeResult name tapeBefore tapeAfter) = "> " <> name <> " on " <> pretty tapeBefore <> " : " <> pretty tapeAfter
