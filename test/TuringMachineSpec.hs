module TuringMachineSpec where

import qualified Data.Map               as M
-- import           Tape
import qualified Code.Internal          as C
import           Tape
import           Test.Hspec
import           TuringMachine.Internal

turingMachineTests :: SpecWith ()
turingMachineTests = describe "Turing Machine" $ do
  it "should build machines from code AST" $ do
    let code = C.Code (
                C.Definition (C.State "s") [C.State "f"]
                  [ C.Rule (C.State "s") (Symbol "0") (C.State "s") (Symbol "1") R
                  , C.Rule (C.State "s") Blank        (C.State "x") Blank        L
                  , C.Rule (C.State "x") (Symbol "1") (C.State "x") (Symbol "1") L
                  , C.Rule (C.State "x") Blank        (C.State "f") Blank        R
                  ])
                [C.Simulation (fromList [Symbol "0", Symbol "0"])]

        (tm, tapes) = fromCode code
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
    toList (tape tm) `shouldBe` []
