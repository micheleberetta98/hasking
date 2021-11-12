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

import           Code.Internal
