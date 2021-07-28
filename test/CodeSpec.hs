module CodeSpec where

import           Code
import qualified Data.Map      as M
import           Tape          hiding (empty)
import           Test.Hspec
import           TuringMachine
import           Validation

codeTests :: SpecWith ()
codeTests = describe "Code" $ do
  it "should parse a correct code" $ do
    let
      Ok code = fromCode "; A comment line\n{0 . 1}\n[BEGIN s1] ; This is the initial state\n[FINAL s2]\n(s1 0 s2 . S)\n      ; Another comment"
      tape = initialTape code
    initialState code `shouldBe` State "s1"
    finalStates code `shouldBe` [State "s2"]
    transitions code `shouldBe` M.fromList [((State "s1", Symbol "0"), (State "s2", Blank, S))]
    value tape `shouldBe` Symbol "0"
    value (move R tape) `shouldBe` Blank
    value (move R (move R tape)) `shouldBe` Symbol "1"

  it "should not parse wrong code" $ do
    let
      Err msg1 = fromCode "[BEGIN s1]\n[FINAL s2]\n(s10 s2 . S)"
      Err msg2 = fromCode "{0 0 0 0}\n[BEGIN s a]\n[FINAL q]\n(s 0 s 1 R)"

    length msg1 `shouldNotBe` 0
    length msg2 `shouldNotBe` 0

