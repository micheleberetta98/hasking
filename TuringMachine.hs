module TuringMachine
  ( machine
  , TMState(..)
  , Transition(..)
  ) where

import           Data.Function (on)
import           Tape          (Movement, Symbol, Tape, get, move, write)

newtype TMState s = TMState { state :: s }
newtype Transition s a = Transition {
  runTransition :: (TMState s, Symbol a) -> Maybe (TMState s, Symbol a, Movement)
}

instance (Eq s) => Eq (TMState s) where
  (==) = (==) `on` state

machine :: (Eq s) => Transition s a -> TMState s -> [TMState s] -> Tape a -> Maybe (Tape a)
machine t s finals tape
  | s `elem` finals = Just tape
  | otherwise       = do
    (s', o, d) <- runTransition t (s, get tape)
    machine t s' finals (move d $ write o tape)
