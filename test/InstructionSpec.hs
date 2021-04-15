module InstructionSpec where

import           Instruction
import           Parser
import           Pretty
import           Test.Hspec


instructionTests :: SpecWith ()
instructionTests = describe "Instruction" $ do
  it "should parse correct steps" $ do
    let
      Right i1 = parseInstruction "(s 0 q 1 R)"
      Right i2 = parseInstruction " ( s  0   q  1 R  ) "
      Right i3 = parseInstruction "(s2 20 qx7 L21 S)"
      Right i4 = parseInstruction "(s1 0 s2 . S)"

    pretty i1 `shouldBe` "(s 0 q 1 R)"
    pretty i2 `shouldBe` "(s 0 q 1 R)"
    pretty i3 `shouldBe` "(s2 20 qx7 L21 S)"
    pretty i4 `shouldBe` "(s1 0 s2 . S)"

  it "should not parse wrong steps" $ do
    let
      Left i1 = parseInstruction ""
      Left i2 = parseInstruction "[s 0 q 1 R]"
      Left i3 = parseInstruction "(12 x 13 y R)"
      Left i4 = parseInstruction "(s 1 q 0 R12)"
      Left i5 = parseInstruction "(s 1 q 0 M)"
      Left i6 = parseInstruction "(s .. q 0 M)"
      Left i7 = parseInstruction "(s .. q 0 M) asd"

    pretty i1 `shouldNotBe` ""
    pretty i2 `shouldNotBe` ""
    pretty i3 `shouldNotBe` ""
    pretty i4 `shouldNotBe` ""
    pretty i5 `shouldNotBe` ""
    pretty i6 `shouldNotBe` ""

  it "should parse correct control sequences" $ do
    let
      Right c1 = parseInstruction "[BEGIN s1]"
      Right c2 = parseInstruction "[FINAL s1 s2 s3]"
      Right c3 = parseInstruction "[FINAL s1]"

    pretty c1 `shouldBe` "[BEGIN s1]"
    pretty c2 `shouldBe` "[FINAL s1 s2 s3]"
    pretty c3 `shouldBe` "[FINAL s1]"

  it "should not parse wrong control sequences" $ do
    let
      Left i1 = parseInstruction ""
      Left i2 = parseInstruction "[s 0 q 1 R]"
      Left i3 = parseInstruction "(BEGIN s)"
      Left i4 = parseInstruction "[begin12 s]"
      Left i5 = parseInstruction "[BEGIN .]"

    pretty i1 `shouldNotBe` ""
    pretty i2 `shouldNotBe` ""
    pretty i3 `shouldNotBe` ""
    pretty i4 `shouldNotBe` ""
    pretty i5 `shouldNotBe` ""

  it "should parse correct tapes" $ do
    let
      Right t1 = parseInstruction "{0 0 2}"
      Right t2 = parseInstruction "{a b c}"
      Right t3 = parseInstruction "{  s   }"
      Right t4 = parseInstruction "{0 0 . 0 0}"

    pretty t1 `shouldBe` "{0 0 2}"
    pretty t2 `shouldBe` "{a b c}"
    pretty t3 `shouldBe` "{s}"
    pretty t4 `shouldBe` "{0 0 . 0 0}"

