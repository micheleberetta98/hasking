module TapeSpec where

import           Tape
import           Test.Hspec

tapeTests :: SpecWith ()
tapeTests = describe "Tape" $ do
  it "should convert from and to a list" $ do
    let tape1 = fromList [Symbol "1", Symbol "2", Symbol "3", Symbol "4"]
    value tape1 `shouldBe` Symbol "1"
    toList tape1 `shouldBe` [Symbol "1", Symbol "2", Symbol "3", Symbol "4"]

  it "should move left and right" $ do
    let tape = fromList [Symbol "1", Symbol "2", Symbol "3", Symbol "4"]
    value (move R tape) `shouldBe` Symbol "2"
    value (move L tape) `shouldBe` Blank
    value (move S tape) `shouldBe` Symbol "1"

    let
      input = fromList [Symbol "I", Symbol "I", Blank, Symbol "X"]
      transform = move R . move R . move R

    value (transform input) `shouldBe` Symbol "X"

  it "should allow writes" $ do
    let tape = write (Symbol "5") $ fromList [Symbol "1", Symbol "2", Symbol "3", Symbol "4"]
    toList tape `shouldBe` [Symbol "5", Symbol "2", Symbol "3", Symbol "4"]

    let tape2 = write (Symbol "5") $ fromList [Blank, Symbol "2", Symbol "3", Symbol "4"]
    toList tape2 `shouldBe` [Symbol "5", Symbol "2", Symbol "3", Symbol "4"]

  it "should move past blanks correctly" $ do
    let
      tape = fromList [Symbol 0, Blank, Symbol 1]
      transform = move R . move R . move R
      output = value (transform tape)

    output `shouldBe` Symbol 1

  it "should allow complicated stuff" $ do
    let
      tape = fromList [Symbol "I", Symbol "I", Blank, Symbol "I"]
      transform = move L . move L . write (Symbol "X") . move R . move R
      output = toList (transform tape)

    output `shouldBe` [Symbol "I", Symbol "I", Symbol "X", Symbol "I"]

  it "should allow more complicated stuff" $ do
    let
      tape = fromList [Symbol "I", Symbol "I", Blank, Symbol "I"]
      transform =move R . write (Blank) . move L . move L . move L . move R . write (Symbol "X") . move R . move R
      output = toList (transform tape)

    output `shouldBe` [Symbol "I", Symbol "X", Symbol "I"]

