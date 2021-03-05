module TuringMachine
  ( machine
  , TMState(..)
  , Transition(..)
  ) where

import           Data.Function (on)
import           Tape          (Movement, Symbol, Tape, move, value, write)

newtype TMState s = TMState { state :: s }
newtype Transition s a = Transition {
  runTransition :: (TMState s, Symbol a) -> Maybe (TMState s, Symbol a, Movement)
}

instance (Eq s) => Eq (TMState s) where
  (==) = (==) `on` state

machine :: (Eq s) => Transition s a -> TMState s -> [TMState s] -> Tape a -> Maybe (Tape a)
machine t st finals tape
  | st `elem` finals = Just tape
  | otherwise       = do
    (st', out, dir) <- runTransition t (st, value tape)
    machine t st' finals (move dir $ write out tape)
