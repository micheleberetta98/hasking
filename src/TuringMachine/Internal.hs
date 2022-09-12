module TuringMachine.Internal where

import           Data.Function
import           Data.Map      (Map, (!?))
import qualified Data.Map      as M
import           Pretty
import           Tape

------------------------------------------------
-- Types
------------------------------------------------

-- | The status of the Turing Machine - either @Running@ or @Stopped@
data Status = Running | Stopped deriving (Show, Eq)

-- | The state of the Turing Machine
newtype State s = State { getState :: s } deriving (Show)

-- | Represents a state in which the machine can be found, along with the read symbol from the tape
type From s a = (State s, Symbol a)

-- | Represents the new state of the machine after a transaction, the symbol to write and the direction to move to
type To s a = (State s, Symbol a, Direction)

-- | All of the transitions
type Transitions s a = Map (From s a) (To s a)

-- | The executable @TuringMachine@
data TuringMachine s a = TuringMachine
  { initial     :: State s
  , finals      :: [State s]
  , transitions :: Transitions s a
  , current     :: State s
  , status      :: Status
  }
  deriving (Show, Eq)

-- | A simple utility for rules
type Rule s a = (State s, Symbol a, State s, Symbol a, Direction)

------------------------------------------------
-- Instances
------------------------------------------------

instance (Eq s) => Eq (State s) where
  (==) = (==) `on` getState

instance (Ord s) => Ord (State s) where
  (<=) = (<=) `on` getState

instance (Pretty s) => Pretty (State s) where
  pretty (State s) = pretty s

------------------------------------------------
-- Construction
------------------------------------------------

mkMachine :: State s -> [State s] -> Transitions s a -> TuringMachine s a
mkMachine from finalStates ts = TuringMachine
  { initial = from
  , finals = finalStates
  , transitions = ts
  , current = from
  , status = Running
  }

------------------------------------------------
-- Execution
------------------------------------------------

-- | Executes a @TuringMachine@ with the specified @Tape@
-- It returns @Left (state, symbol)@ if it ends up in an undefined state, or the resulting @Right (TuringMachine, Tape)@
runMachine :: (Ord s, Ord a) => TuringMachine s a -> Tape a -> Either (From s a) (TuringMachine s a, Tape a)
runMachine tm t
  | status tm == Stopped = Right (tm, t)
  | otherwise            = step tm t >>= uncurry runMachine

-- | Given a particular @TuringMachine@, it runs a transition giving maybe a new @(TuringMachine, Tape)@
-- If the transition has not been defined, it returns @Left (From s a)@
step :: (Ord s, Ord a) => TuringMachine s a -> Tape a -> Either (From s a) (TuringMachine s a, Tape a)
step tm@TuringMachine{ status = Stopped } t = Right (tm, t)
step tm t                                   =
  let c = current tm
      val = value t
  in case transition (c, val) (transitions tm) of
    Nothing -> Left (c, val)
    Just (s', out, dir) ->
      let tm' = tm{ current = s', status = if s' `elem` finals tm then Stopped else Running }
      in Right (tm', move dir $ write out t)

-- | Lookups a single transition from a @Transitions s a@
transition :: (Ord s, Ord a) => From s a -> Transitions s a -> Maybe (To s a)
transition from ts = ts !? from

------------------------------------------------
-- Utilities
------------------------------------------------

-- -- | Builds the transition Map from a list of @Rule@s
buildTransitions :: [Rule String String] -> Transitions String String
buildTransitions = M.fromList . map formatRule
  where
    formatRule (fromState, fromSymbol, toState, toSymbol, dir) = ((fromState, fromSymbol), (toState, toSymbol, dir))
