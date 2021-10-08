module TuringMachine
  ( machine
  , runTransition
  , updateTape
  , State(..)
  , From
  , To
  , Transitions
  ) where

import           Data.Function (on)
import           Data.Map      (Map, (!?))
import           Pretty        (Pretty (..))
import           Tape          (Direction, Symbol, Tape, move, value, write)

------------------------------------------------
-- Types
------------------------------------------------

-- | The state of the Turing Machine
newtype State s = State { getState :: s } deriving (Show)

-- | Represents a state in which the machine can be found, along with the read symbol from the tape
type From s a = (State s, Symbol a)

-- | Represents the new state of the machine after a transaction, the symbol to write and the direction to move to
type To s a = (State s, Symbol a, Direction)

-- | All of the transitions
type Transitions s a = Map (From s a) (To s a)

------------------------------------------------
-- The machine
------------------------------------------------

-- | Executes a machine with the following
-- - The @Transitions@
-- - An initial @State@
-- - A list of final @State@s
-- - The @Tape@ to analyze
-- It returns @Nothing@ if it ends up in an undefined state, or @Just@ the resulting @Tape@
machine :: (Ord s, Eq s, Ord a) =>
  Transitions s a
  -> State s
  -> [State s]
  -> Tape a
  -> Either (From s a) (Tape a)
machine t st finals tape
  | st `elem` finals = Right tape
  | otherwise        = do
    (st', out, dir) <- runTransition t (st, value tape)
    machine t st' finals (updateTape dir out tape)

------------------------------------------------
-- Transitions and rules
------------------------------------------------

-- | Given a particular @From@, it runs a transition giving maybe a @To@
-- If the transition has not been defined, it returns @Nothing@
runTransition :: (Ord s, Ord a) => Transitions s a -> From s a -> Either (From s a) (To s a)
runTransition transitions from =
  case transitions !? from of
    Just to -> Right to
    Nothing -> Left from

-- | It updates a given tape by writing @value@ at the current position
-- and moving @dir@
updateTape :: Direction -> Symbol a -> Tape a -> Tape a
updateTape dir value = move dir . write value

------------------------------------------------
-- Instances
------------------------------------------------

instance (Eq s) => Eq (State s) where
  (==) = (==) `on` getState

instance (Ord s) => Ord (State s) where
  (<=) = (<=) `on` getState

instance (Pretty s) => Pretty (State s) where
  pretty (State s) = pretty s
