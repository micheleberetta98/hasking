module InstructionSpec where

import           Instruction
import           Parser
import           Test.Hspec


instructionTests = describe "Instruction" $ do
  it "should parse correct instructions" $ do
    let
      Just (i1, _) = runParser parseInstruction "(s 0 q 1 L)"
      Just (i2, _) = runParser parseInstruction " ( s  0   q  1 R  ) "
      Just (i3, _) = runParser parseInstruction "(s2 20 qx7 L21 S)"

    show i1 `shouldBe` "(s 0 q 1 L)"
    show i2 `shouldBe` "(s 0 q 1 R)"
    show i3 `shouldBe` "(s2 20 qx7 L21 S)"

  it "should not parse wrong instructions" $ do
    let
      i1 = runParser parseInstruction ""
      i2 = runParser parseInstruction "[s 0 q 1 R]"
      i3 = runParser parseInstruction "(12 x 13 y R)"
      i4 = runParser parseInstruction "(s 1 q 0 R12)"
      i5 = runParser parseInstruction "(s 1 q 0 M)"

    i1 `shouldBe` Nothing
    i2 `shouldBe` Nothing
    i3 `shouldBe` Nothing
    i4 `shouldBe` Nothing
    i5 `shouldBe` Nothing
