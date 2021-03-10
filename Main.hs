module Main where

import           Tape          (Direction (..), Symbol (..), fromList, toList)
import           TuringMachine (State (..), Transition (..), machine)

main :: IO ()
main = do
  let
    tape = fromList [0, 1, 0, 1]
    result = machine t (State 'A') [State 'C'] tape
  print (toList <$> result)
  print (result >> Just "OK")

t :: Transition Char Int
t = Transition t'
  where
    t' (State 'A', Symbol _) = Just (State 'A', Symbol 1, R)
    t' (State 'A', Blank)    = Just (State 'B', Blank, L)
    t' (State 'B', Blank)    = Just (State 'C', Blank, R)
    t' (State 'B', x)        = Just (State 'B', x, L)
    t' _                     = Nothing
