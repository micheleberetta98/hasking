module TuringMachine
  ( machine
  , State(..)
  , Transition(..)
  ) where

import           Data.Function (on)
import           Tape          (Movement, Symbol, Tape, move, value, write)

newtype State s = State { getState :: s }
newtype Transition s a = Transition {
  runTransition :: (State s, Symbol a) -> Maybe (State s, Symbol a, Movement)
}

instance (Eq s) => Eq (State s) where
  (==) = (==) `on` getState

machine :: (Eq s) => Transition s a -> State s -> [State s] -> Tape a -> Maybe (Tape a)
machine t st finals tape
  | st `elem` finals = Just tape
  | otherwise        = do
    (st', out, dir) <- runTransition t (st, value tape)
    machine t st' finals (move dir $ write out tape)
