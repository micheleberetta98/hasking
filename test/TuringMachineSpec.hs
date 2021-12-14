module TuringMachineSpec where

import qualified Data.Map               as M
import           Tape
import           Test.Hspec
import           TuringMachine.Internal

turingMachineTests :: SpecWith ()
turingMachineTests = describe "Turing Machine" $ do
  it "should build machines from code AST" $ do
    let tm = TuringMachine
                (State "s")
                [State "f"]
                ( buildTransitions
                  [ (State "s", Symbol "0", State "s", Symbol "1", R)
                  , (State "s", Blank,      State "x", Blank,      L)
                  , (State "x", Symbol "1", State "x", Symbol "1", L)
                  , (State "x", Blank,      State "f", Blank,      R)
                  ])
                (State "s")
                Running
        tapes = [fromList [Symbol "0", Symbol "0"]]
    initial tm `shouldBe` State "s"
    finals tm `shouldBe` [State "f"]
    transitions tm `shouldBe` M.fromList
      [ ((State "s", Symbol "0"), (State "s", Symbol "1", R))
      , ((State "s", Blank),      (State "x", Blank,      L))
      , ((State "x", Symbol "1"), (State "x", Symbol "1", L))
      , ((State "x", Blank),      (State "f", Blank,      R))
      ]
    current tm `shouldBe` initial tm
    status tm `shouldBe` Running
