module Code.Internal where

import           Pretty
import           Tape

-----------------------------------------------
-- Types
-----------------------------------------------

data Code = Code
  { definition  :: Definition
  , simulations :: [Simulation]
  } deriving (Show)

newtype Simulation = Simulation { getSimulationTape :: Tape String }
  deriving (Show)

newtype State = State { getState :: String }
  deriving (Eq, Show)

data Definition = Definition
  { initialState :: State
  , finalStates  :: [State]
  , rules        :: [Rule]
  } deriving (Eq, Show)

data Rule = Rule State (Symbol String) State (Symbol String) Direction
  deriving (Eq, Show)

-----------------------------------------------
-- Instances
-----------------------------------------------

instance Pretty Code where
  pretty (Code def sims) = unlines $ [pretty def, unlines $ map pretty sims]

instance Pretty Definition where
  pretty (Definition i fs rs) = unlines
    [ "(machine"
    , "  (initial " ++ pretty i ++ ")"
    , "  (finals (" ++ unwords (map pretty fs) ++ "))"
    , "  (rules ("
    , unlines $ map (("    " ++) . pretty) rs
    , "  ))"
    ]
instance Pretty Simulation where
  pretty (Simulation t) = "(simulate-on (" ++ pretty t ++ "))"

instance Pretty State where
  pretty (State s) = s

instance Pretty Rule where
  pretty (Rule s1 y1 s2 y2 d) = "(" ++ unwords [pretty s1, pretty y1, pretty s2, pretty y2, pretty d] ++ ")"

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
