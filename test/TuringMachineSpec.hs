module TuringMachineSpec where

import qualified Data.Map      as M
import           Tape
import           Test.Hspec
import           TuringMachine

turingMachineTests :: SpecWith ()
turingMachineTests = describe "Turing Machine" $ do
  it "should give back the tape when everything goes right (1)" $ do
    let
      tape = fromList [Symbol 0, Symbol 0, Symbol 0, Symbol 0]
      result = machine t (State "A") [State "C"] tape
    toList <$> result `shouldBe` Right [Symbol 1, Symbol 1, Symbol 1, Symbol 1]

  it "should give back the tape when everything goes right (2)" $ do
    let
      tape = fromList [Symbol 0, Blank, Symbol 1]
      result = machine t (State "A") [State "C"] tape
    toList <$> result `shouldBe` Right [Symbol 1]

  it "should give back Nothing when something goes wrong" $ do
    let
      tape = fromList [Symbol 0, Symbol 1, Symbol 0, Symbol 0]
      result = machine t (State "A") [State "C"] tape
    toList <$> result `shouldBe` Left (State "A", Symbol 1)

t :: Transitions String Int
t = M.fromList
  [ ((State "A", Symbol 0), (State "A", Symbol 1, R))
  , ((State "A", Blank   ), (State "B", Blank,    L))
  , ((State "B", Blank   ), (State "C", Blank,    R))
  , ((State "B", Symbol 0), (State "B", Symbol 0, L))
  , ((State "B", Symbol 1), (State "B", Symbol 1, L))
  ]

t' = M.fromList
  [ ((State "A", Symbol 0), (State "A", Blank, R))
  , ((State "A", Blank),    (State "B", Blank, R))
  , ((State "B", Symbol 1), (State "B", Blank, R))
  , ((State "C", Blank),    (State "C", Symbol 1, S))
  ]
