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

import           TuringMachine.Internal
