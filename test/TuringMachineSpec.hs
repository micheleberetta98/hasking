module TuringMachineSpec where

import           Tape
import           Test.Hspec
import           TuringMachine

turingMachineTests :: SpecWith ()
turingMachineTests = describe "Turing Machine" $ do
  it "should give back the tape when everything goes right" $ do
    let
      tape = fromList [0, 0, 0, 0]
      result = machine t (State "A") [State "C"] tape
    toList <$> result `shouldBe` Right [1, 1, 1, 1]

  it "should give back Nothing when something goes wrong" $ do
    let
      tape = fromList [0, 1, 0, 0]
      result = machine t (State "A") [State "C"] tape
    toList <$> result `shouldBe` Left (State "A", Symbol 1)

t :: Transitions String Int
t = buildTransitions
  [ ((State "A", Symbol 0), (State "A", Symbol 1, R))
  , ((State "A", Blank   ), (State "B", Blank,    L))
  , ((State "B", Blank   ), (State "C", Blank,    R))
  , ((State "B", Symbol 0), (State "B", Symbol 0, L))
  , ((State "B", Symbol 1), (State "B", Symbol 1, L))
  ]

