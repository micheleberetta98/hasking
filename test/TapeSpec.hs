module TapeSpec where

import           Tape
import           Test.Hspec

tapeTests :: SpecWith ()
tapeTests = describe "Tape" $ do
  it "should convert from and to a list" $ do
    let tape = fromList [1, 2, 3, 4]
    value tape `shouldBe` Symbol 1
    toList tape `shouldBe` [1, 2, 3, 4]

  it "should move left and right" $ do
    let tape = fromList [1, 2, 3, 4]
    value (move R tape) `shouldBe` Symbol 2
    value (move L tape) `shouldBe` Blank
    value (move S tape) `shouldBe` Symbol 1

  it "should allow writes" $ do
    let tape = write (Symbol 5) $ fromList [1, 2, 3, 4]
    toList tape `shouldBe` [5, 2, 3, 4]
