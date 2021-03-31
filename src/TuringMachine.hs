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
import           Pretty        (Pretty (..))
import           Tape          (Direction, Symbol, Tape, move, value, write)

------------------------------------------------
-- Types
------------------------------------------------

-- | The state of the Turing Machine
newtype State s = State { getState :: s } deriving (Show)

-- | A list of states
type StateList s = [State s]

-- | Represents a state in which the machine can be found, along with the read symbol from the tape
type FromState s a = (State s, Symbol a)

-- | Represents the new state of the machine after a transaction, the symbol to write and the direction to move to
type ToState s a = (State s, Symbol a, Direction)

-- | All of the transitions
type Transitions s a = Map (FromState s a) (ToState s a)

------------------------------------------------
-- The machine
------------------------------------------------

-- | Executes a machine with the following
-- - The `Transitions`
-- - An initial `State`
-- - A list of final `State`s
-- - The `Tape` to analyze
-- It returns `Nothing` if it ends up in an undefined state, or `Just` the resulting `Tape`
machine :: (Ord s, Eq s, Ord a) =>
  Transitions s a
  -> State s
  -> StateList  s
  -> Tape a
  -> Either (FromState s a) (Tape a)
machine t st finals tape
  | st `elem` finals = Right tape
  | otherwise        = do
    (st', out, dir) <- runTransition t (st, value tape)
    machine t st' finals (move dir $ write out tape)

------------------------------------------------
-- Transitions and rules
------------------------------------------------

-- | Given a particular `FromState`, it runs a transition giving maybe a `ToState`
-- If the transition has not been defined, it returns `Nothing`
runTransition :: (Ord s, Ord a) => Transitions s a -> FromState s a -> Either (FromState s a) (ToState s a)
runTransition transitions fromState =
  case transitions !? fromState of
    Just to -> Right to
    Nothing -> Left fromState

-- | Constructs the transitions from a list of `(FromState s a, ToState s a)`
buildTransitions :: (Ord s, Ord a) => [(FromState s a, ToState s a)] -> Transitions s a
buildTransitions = fromList

------------------------------------------------
-- Instances
------------------------------------------------

instance (Eq s) => Eq (State s) where
  (==) = (==) `on` getState

instance (Ord s) => Ord (State s) where
  (<=) = (<=) `on` getState

instance (Pretty s) => Pretty (State s) where
  pretty (State s) = pretty s
