module TuringMachineSpec where

import qualified Data.Map               as M
import           Pretty
import           Tape
import           Test.Hspec
import           TuringMachine.Internal

turingMachineTests :: SpecWith ()
turingMachineTests = describe "Turing Machine" $ do
  let tm = TuringMachine
                (State "s")
                [State "f"]
                ( buildTransitions
                  [ (State "s", Symbol '0', State "s", Symbol '1', R)
                  , (State "s", Blank,      State "x", Blank,      L)
                  , (State "x", Symbol '1', State "x", Symbol '1', L)
                  , (State "x", Blank,      State "f", Blank,      R)
                  ])
                (State "s")
                Running
  let tape1 = fromList [Symbol '0', Symbol '0']
      tape2 = fromList [Symbol '1', Symbol '0']

  it "should build machines from code AST" $ do
    initial tm `shouldBe` State "s"
    finals tm `shouldBe` [State "f"]
    transitions tm `shouldBe` M.fromList
      [ ((State "s", Symbol '0'), (State "s", Symbol '1', R))
      , ((State "s", Blank),      (State "x", Blank,      L))
      , ((State "x", Symbol '1'), (State "x", Symbol '1', L))
      , ((State "x", Blank),      (State "f", Blank,      R))
      ]
    current tm `shouldBe` initial tm
    status tm `shouldBe` Running

  it "should execute the whole machine" $ do
    pretty . snd <$> runMachine tm tape1 `shouldBe` Right "(1 1)"
    runMachine tm tape2 `shouldBe` Left (State "s", Symbol '1')
