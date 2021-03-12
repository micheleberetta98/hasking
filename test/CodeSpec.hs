module CodeSpec where

import           Code
import qualified Data.Map      as M
import           Tape
import           Test.Hspec
import           TuringMachine

codeTests :: SpecWith ()
codeTests = describe "Code" $ do
  it "should parse a correct code" $ do
    let Just code = parseCode "# A comment line\n{0 0}\n[BEGIN s1] # This is the initial state\n[FINAL s2]\n(s1 0 s2 . S)"
    initialState code `shouldBe` State "s1"
    finalStates code `shouldBe` [State "s2"]
    transitions code `shouldBe` M.fromList [((State "s1", Symbol "0"), (State "s2", Blank, S))]

  it "should not parse wrong code" $ do
    let code = parseCode "[BEGIN s1]\n[FINAL s2]\n(s10 s2 . S)"
    code `shouldBe` Nothing

