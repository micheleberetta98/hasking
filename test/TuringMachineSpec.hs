module TuringMachineSpec where

import           Tape
import           Test.Hspec
import           TuringMachine

turingMachineTests :: SpecWith ()
turingMachineTests = describe "Turing Machine" $ do
  it "should give back the tape when everything goes right" $ do
    let
      tape = fromList [0, 0, 0, 0]
      result = machine t (State 'A') [State 'C'] tape
    toList <$> result `shouldBe` Just [1, 1, 1, 1]

  it "should give back Nothing when something goes wrong" $ do
    let
      tape = fromList [0, 1, 0, 0]
      result = machine t (State 'A') [State 'C'] tape
    toList <$> result `shouldBe` Nothing

t :: Transition Char Int
t = Transition t'
  where
    t' (State 'A', Symbol 0) = Just (State 'A', Symbol 1, R)
    t' (State 'A', Blank)    = Just (State 'B', Blank, L)
    t' (State 'B', Blank)    = Just (State 'C', Blank, R)
    t' (State 'B', x)        = Just (State 'B', x, L)
    t' _                     = Nothing
