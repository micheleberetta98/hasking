module TuringMachine
  ( machine
  , State(..)
  , FromState
  , ToState
  , StateList
  , Transitions
  , buildTransitions
  ) where

import           Data.Function (on)
import           Data.Map      (Map, fromList, (!?))
import           Tape          (Direction, Symbol, Tape, move, value, write)

------------------------------------------------
-- Types
------------------------------------------------

-- | The state of the Turing Machine
newtype State = State { getState :: String }

-- | A list of states
type StateList = [State]

-- | Represents a state in which the machine can be found, along with the read symbol from the tape
type FromState a = (State, Symbol a)

-- | Represents the new state of the machine after a transaction, the symbol to write and the direction to move to
type ToState a = (State, Symbol a, Direction)

-- | All of the transitions
type Transitions a = Map (FromState a) (ToState a)

------------------------------------------------
-- The machine
------------------------------------------------

-- | Executes a machine with the following
-- - The `Transitions`
-- - An initial `State`
-- - A list of final `State`s
-- - The `Tape` to analyze
-- It returns `Nothing` if it ends up in an undefined state, or `Just` the resulting `Tape`
machine :: (Ord a) => Transitions a -> State -> StateList -> Tape a -> Maybe (Tape a)
machine t st finals tape
  | st `elem` finals = Just tape
  | otherwise        = do
    (st', out, dir) <- runTransition t (st, value tape)
    machine t st' finals (move dir $ write out tape)

------------------------------------------------
-- Transitions and rules
------------------------------------------------

-- | Given a particular `FromState`, it runs a transition giving maybe a `ToState`
-- If the transition has not been defined, it returns `Nothing`
runTransition :: (Ord a) => Transitions a -> FromState a -> Maybe (ToState a)
runTransition transitions fromState = transitions !? fromState

-- | Constructs the transitions from a list of `(FromState a, ToState a)`
buildTransitions :: (Ord a) => [(FromState a, ToState a)] -> Transitions a
buildTransitions = fromList

------------------------------------------------
-- Instances
------------------------------------------------

instance Eq State where
  (==) = (==) `on` getState

instance Ord State where
  (<=) = (<=) `on` getState
