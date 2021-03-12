module InstructionSpec where

import           Instruction
import           Parser
import           Test.Hspec


instructionTests :: SpecWith ()
instructionTests = describe "Instruction" $ do
  it "should parse correct steps" $ do
    let
      Right i1 = parseInstruction "(s 0 q 1 R)"
      Right i2 = parseInstruction " ( s  0   q  1 R  ) "
      Right i3 = parseInstruction "(s2 20 qx7 L21 S)"
      Right i4 = parseInstruction "(s . q . S)"

    show i1 `shouldBe` "(s 0 q 1 R)"
    show i2 `shouldBe` "(s 0 q 1 R)"
    show i3 `shouldBe` "(s2 20 qx7 L21 S)"
    show i4 `shouldBe` "(s . q . S)"

  it "should not parse wrong steps" $ do
    let
      Left i1 = parseInstruction ""
      Left i2 = parseInstruction "[s 0 q 1 R]"
      Left i3 = parseInstruction "(12 x 13 y R)"
      Left i4 = parseInstruction "(s 1 q 0 R12)"
      Left i5 = parseInstruction "(s 1 q 0 M)"
      Left i6 = parseInstruction "(s .. q 0 M)"
      Left i7 = parseInstruction "(s .. q 0 M) asd"

    i1 `shouldNotBe` ""
    i2 `shouldNotBe` ""
    i3 `shouldNotBe` ""
    i4 `shouldNotBe` ""
    i5 `shouldNotBe` ""
    i6 `shouldNotBe` ""

  it "should parse correct control sequences" $ do
    let
      Right c1 = parseInstruction "[BEGIN s1]"
      Right c2 = parseInstruction "[FINAL s1 s2 s3]"

    show c1 `shouldBe` "[BEGIN s1]"
    show c2 `shouldBe` "[FINAL s1 s2 s3]"

  it "should not parse wrong control sequences" $ do
    let
      Left i1 = parseInstruction ""
      Left i2 = parseInstruction "[s 0 q 1 R]"
      Left i3 = parseInstruction "(BEGIN s)"
      Left i4 = parseInstruction "[begin12 s]"
      Left i5 = parseInstruction "[FINAL]"
      Left i6 = parseInstruction "[BEGIN .]"

    i1 `shouldNotBe` ""
    i2 `shouldNotBe` ""
    i3 `shouldNotBe` ""
    i4 `shouldNotBe` ""
    i5 `shouldNotBe` ""
    i6 `shouldNotBe` ""

  it "should parse correct tapes" $ do
    let
      Right t1 = parseInstruction "{0 0 2}"
      Right t2 = parseInstruction "{a b c}"
      Right t3 = parseInstruction "{  s   }"

    show t1 `shouldBe` "{0 0 2}"
    show t2 `shouldBe` "{a b c}"
    show t3 `shouldBe` "{s}"

