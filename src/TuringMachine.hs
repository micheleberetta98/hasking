module TuringMachine
  ( TuringMachine(..)
  , Status(..)
  , From
  , To
  , machine
  , step
  , transition
  , currentFrom
  , fromCode
  , withTape
  )
where

import           Code          hiding (State)
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
  , transitions :: Map (From s a) (To s a)
  , current     :: State s
  , status      :: Status
  , tape        :: Tape a
  }

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
-- Interface
------------------------------------------------

-- | Executes a @TuringMachine@ with the specified @Tape@
-- It returns @Left (state, symbol)@ if it ends up in an undefined state, or the resulting @Right TuringMachine@
machine :: (Ord s, Ord a) => TuringMachine s a -> Tape a -> Either (From s a) (TuringMachine s a)
machine tm tape = machine' $ withTape tape tm
  where
    machine' tm@TuringMachine{ status = Stopped } = Right tm
    machine' tm                                   = step tm >>= machine'

-- | Given a particular @TuringMachine@, it runs a transition giving maybe a new @TuringMachine@
-- If the transition has not been defined, it returns @Left (From s a)@
step :: (Ord s, Ord a) => TuringMachine s a -> Either (From s a) (TuringMachine s a)
step tm@(TuringMachine _ _ _ _ Stopped _)     = Right tm
step tm@(TuringMachine _ fs ts cur _ tape) =
  let from = currentFrom tm in
  case transition from ts of
    Nothing              -> Left from
    Just (st', out, dir) -> Right tm
                                  { current = st'
                                  , tape = updateTape dir out tape
                                  , status = if st' `elem` fs then Stopped else Running
                                  }

-- | Lookups a single transition from a @Transitions s a@
transition :: (Ord s, Ord a) => From s a -> Transitions s a -> Maybe (To s a)
transition from ts = ts !? from

-- | Gives the current @State s@ and @Symbol a@
currentFrom :: TuringMachine s a -> From s a
currentFrom m = (current m, value $ tape m)

-- | Converts a @Code@ into a @TuringMachine String String@, returning also the simulations (i.e. @Tape String@s)
fromCode :: Code -> (TuringMachine String String, [Tape String])
fromCode code = (m, tapes)
  where
    m = TuringMachine
      { initial = State initialState
      , finals = map State finalStates
      , transitions = buildTransitions rules
      , current = State initialState
      , status = Running
      , tape = empty
      }
    tapes = map getSimulationTape (simulations code)

    (initialState, finalStates, rules) = getDefinitions $ definition code

------------------------------------------------
-- Utilities
------------------------------------------------

-- | Overrides the @TuringMachine@'s @Tape@
withTape :: Tape a -> TuringMachine s a -> TuringMachine s a
withTape t tm = tm{ tape = t }

-- | Builds the transition Map from a list of @Code.Rule@
buildTransitions :: [Rule] -> Transitions String String
buildTransitions = M.fromList . map formatRule
  where
    formatRule r =
      let (fromState, fromSymbol) = getRuleFrom r
          (toState, toSymbol, d) = getRuleTo r
      in ((State fromState, fromSymbol), (State toState, toSymbol, d))

-- | It updates a given tape by writing @value@ at the current position
-- and moving @dir@
updateTape :: Direction -> Symbol a -> Tape a -> Tape a
updateTape dir value = move dir . write value
