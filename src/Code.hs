module Code where

import           Tape (Direction, Symbol, Tape)

newtype State s = State { getState :: s } deriving (Show)

data Rule = Rule (State String) (Symbol String) (State String) (Symbol String) Direction
  deriving (Show)

data Definition = Definition
  { initialState :: State String
  , finalStates  :: [State String]
  , rules        :: [Rule]
  } deriving (Show)

newtype Simulate = Simulate (Tape String) deriving (Show)

data Code = Code Definition [Simulate] deriving (Show)


