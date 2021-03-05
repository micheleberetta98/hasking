module Main where

import           Tape          (Movement (..), Symbol (..), Tape, fromList,
                                toList)
import           TuringMachine (TMState (..), Transition (..), machine)

type State = TMState Char

main :: IO ()
main = do
  let
    tape :: Tape Int
    tape = fromList [0, 1, 0, 1]
    result = machine t (TMState 'A') [TMState 'C'] tape
  print (toList <$> result)
  print (result >> Just "OK")

t :: Transition Char Int
t = Transition t'
  where
    t' (TMState 'A', Symbol _) = Just (TMState 'A', Symbol 1, MRight)
    t' (TMState 'A', Blank)    = Just (TMState 'B', Blank, MLeft)
    t' (TMState 'B', Blank)    = Just (TMState 'C', Blank, MRight)
    t' (TMState 'B', x)        = Just (TMState 'B', x, MLeft)
    t' _                       = Nothing
