module Main where

import           Tape          (Direction (..), Symbol (..), fromList, toList)
import           TuringMachine (State (..), Transitions, buildTransitions,
                                machine)

main :: IO ()
main = do
  let
    tape = fromList [0, 1, 0, 1]
    result = machine t (State "A") [State "C"] tape
  print (toList <$> result)
  print (result >> Just "OK")

t :: Transitions Int
t = buildTransitions
  [ ((State "A", Symbol 0), (State "A", Symbol 1, R))
  , ((State "A", Blank   ), (State "B", Blank,    L))
  , ((State "B", Blank   ), (State "C", Blank,    R))
  , ((State "B", Symbol 0), (State "B", Symbol 0, L))
  , ((State "B", Symbol 1), (State "B", Symbol 1, L))
  ]

