module TuringMachine
  ( machine
  , State(..)
  , Transition(..)
  ) where

import           Data.Function (on)
import           Tape          (Direction, Symbol, Tape, move, value, write)

-- | The state of the Turing Machine
newtype State s = State { getState :: s }

-- | Represents a transition from a state `s` when reading a symbol `a` to another state `a`,
-- writing another symbol `a` and specifying a `Direction`. It returns `Nothing` if the transition
-- is not defined
newtype Transition s a = Transition {
  runTransition :: (State s, Symbol a) -> Maybe (State s, Symbol a, Direction)
}

instance (Eq s) => Eq (State s) where
  (==) = (==) `on` getState

-- | Executes a machine with the following
-- - A `Transition` function
-- - An initial `State`
-- - A list of final `State`s
-- - The `Tape` to analyze
-- It returns `Nothing` if it ends up in an undefined state, or `Just` the resulting `Tape`
machine :: (Eq s) => Transition s a -> State s -> [State s] -> Tape a -> Maybe (Tape a)
machine t st finals tape
  | st `elem` finals = Just tape
  | otherwise        = do
    (st', out, dir) <- runTransition t (st, value tape)
    machine t st' finals (move dir $ write out tape)
