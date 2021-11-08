module Code
  ( Code
  , Definition
  , State
  , Rule
  , Simulation(getSimulationTape)
  , mkCode
  , mkDefinition
  , mkSimulation
  , mkState
  , mkRule
  , addSimulation
  , getDefinitions
  , getSimulations
  , getRuleFrom
  , getRuleTo
  )
where

import           Tape

-----------------------------------------------
-- Types
-----------------------------------------------

data Code = Code
  { definition  :: Definition
  , simulations :: [Simulation]
  } deriving (Show)

newtype Simulation = Simulation { getSimulationTape :: Tape String } deriving (Show)

newtype State = State { getState :: String } deriving (Show)

data Definition = Definition
  { initialState :: State
  , finalStates  :: [State]
  , rules        :: [Rule]
  } deriving (Show)

data Rule = Rule State (Symbol String) State (Symbol String) Direction
  deriving (Show)

-----------------------------------------------
-- Interface
-----------------------------------------------

-- | Builds the @Code@
mkCode :: Definition -> [Simulation] -> Code
mkCode = Code

-- | Constructs a @Definition@
mkDefinition :: State -> [State] -> [Rule] -> Definition
mkDefinition = Definition

-- | Builds a @State@
mkState :: String -> State
mkState = State

-- | Builds a @Rule@
mkRule :: State -> Symbol String -> State -> Symbol String -> Direction -> Rule
mkRule = Rule

-- | Builds a simulation from a given @Tape String@
mkSimulation :: Tape String -> Simulation
mkSimulation = Simulation

-- | Add a @Tape String@ to the @Code@
addSimulation :: Tape String -> Code -> Code
addSimulation t (Code def sims) = Code def (mkSimulation t : sims)

getDefinitions :: Code -> (String, [String], [Rule])
getDefinitions (Code (Definition initial finals rs) _) = (getState initial, map getState finals, rs)

getSimulations :: Code -> [Simulation]
getSimulations (Code _ sims) = sims

getRuleFrom :: Rule -> (String, Symbol String)
getRuleFrom (Rule state symbol _ _ _) = (getState state, symbol)

getRuleTo :: Rule -> (String, Symbol String, Direction)
getRuleTo (Rule _ _ state symbol d) = (getState state, symbol, d)
