module InstructionSpec where

import           Instruction
import           Parser
import           Test.Hspec


instructionTests = describe "Instruction" $ do
  it "should parse correct steps" $ do
    let
      Just (i1, _) = runParser parseInstruction "(s 0 q 1 L)"
      Just (i2, _) = runParser parseInstruction " ( s  0   q  1 R  ) "
      Just (i3, _) = runParser parseInstruction "(s2 20 qx7 L21 S)"
      Just (i4, _) = runParser parseInstruction "(s . q . S)"

    show i1 `shouldBe` "(s 0 q 1 L)"
    show i2 `shouldBe` "(s 0 q 1 R)"
    show i3 `shouldBe` "(s2 20 qx7 L21 S)"
    show i4 `shouldBe` "(s . q . S)"

  it "should not parse wrong steps" $ do
    let
      i1 = runParser parseInstruction ""
      i2 = runParser parseInstruction "[s 0 q 1 R]"
      i3 = runParser parseInstruction "(12 x 13 y R)"
      i4 = runParser parseInstruction "(s 1 q 0 R12)"
      i5 = runParser parseInstruction "(s 1 q 0 M)"
      i6 = runParser parseInstruction "(s .. q 0 M)"

    i1 `shouldBe` Nothing
    i2 `shouldBe` Nothing
    i3 `shouldBe` Nothing
    i4 `shouldBe` Nothing
    i5 `shouldBe` Nothing
    i6 `shouldBe` Nothing

  it "should parse correct control sequences" $ do
    let
      Just (c1, _) = runParser parseInstruction "[BEGIN s1]"
      Just (c2, _) = runParser parseInstruction "[FINAL s1 s2 s3]"

    show c1 `shouldBe` "[BEGIN s1]"
    show c2 `shouldBe` "[FINAL s1 s2 s3]"

  it "should not parse wrong control sequences" $ do
    let
      i1 = runParser parseInstruction ""
      i2 = runParser parseInstruction "[s 0 q 1 R]"
      i3 = runParser parseInstruction "(BEGIN s)"
      i4 = runParser parseInstruction "[begin12 s]"
      i5 = runParser parseInstruction "[FINAL]"
      i6 = runParser parseInstruction "[BEGIN .]"

    i1 `shouldBe` Nothing
    i2 `shouldBe` Nothing
    i3 `shouldBe` Nothing
    i4 `shouldBe` Nothing
    i5 `shouldBe` Nothing
    i6 `shouldBe` Nothing
