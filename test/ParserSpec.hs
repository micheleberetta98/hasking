module ParserSpec where

import           Control.Applicative
import           Parser
import           Tape
import           Test.Hspec

parserTests :: SpecWith ()
parserTests = describe "Parser" $ do
  it "can parse chars" $ do
    let
      ok = runParser (char 'c') "cjhk"
      notok = runParser (char 'x') "(x)"
    ok `shouldBe` Just ('c', "jhk")
    notok `shouldBe` Nothing

  it "can parse integers" $ do
    let
      ok = runParser integer "123 rest"
      notok = runParser integer "rest123"
    ok `shouldBe` Just (123, " rest")
    notok `shouldBe` Nothing

  it "should parse spaces and spaced things" $ do
    runParser spaces "" `shouldBe` Just ("", "")
    runParser spaces "   x" `shouldBe` Just ("   ", "x")
    runParser spaces "x" `shouldBe` Just("", "x")
    runParser spaces1 "" `shouldBe` Nothing
    runParser spaces1 "   x" `shouldBe` Just ("   ", "x")
    runParser spaces1 "x" `shouldBe` Nothing

    runParser (spaced integer) "  12  " `shouldBe` Just (12, "")
    runParser (spaced integer) "12  " `shouldBe` Just (12, "")
    runParser (spaced integer) "  12" `shouldBe` Just (12, "")
    runParser (spaced integer) "12" `shouldBe` Just (12, "")
    runParser (spaced integer) "  x12  " `shouldBe` Nothing

    runParser (spaced1 integer) "  12  " `shouldBe` Just (12, "")
    runParser (spaced1 integer) "12  " `shouldBe` Just (12, "")
    runParser (spaced1 integer) "  12" `shouldBe` Nothing
    runParser (spaced1 integer) "12" `shouldBe` Nothing
    runParser (spaced1 integer) "x12 " `shouldBe` Nothing

  it "should parse pure strings" $ do
    runParser astring "BEGIN" `shouldBe` Just ("BEGIN", "")
    runParser astring "BEGIN123" `shouldBe` Just ("BEGIN", "123")
    runParser astring "123" `shouldBe` Nothing

  it "should parse identifiers" $ do
    runParser identifier "stateName123,xyz" `shouldBe` Just ("stateName123", ",xyz")
    runParser identifier "123state,xyz" `shouldBe` Nothing

  it "should combine neatly" $ do
    runParser (identifier <|> spaces1) "k123.." `shouldBe` Just ("k123", "..")
    runParser (identifier <|> spaces1) "   k123.." `shouldBe` Just ("   ", "k123..")
    runParser (identifier <|> spaces1) "12465" `shouldBe` Nothing

    runParser (spaces *> identifier <* spaces) "   k123   " `shouldBe` Just ("k123", "")
    runParser (spaces *> identifier <* spaces) "k123" `shouldBe` Just ("k123", "")
    runParser (spaces *> identifier <* spaces) "  456  " `shouldBe` Nothing
    runParser (spaces *> identifier <* spaces) "" `shouldBe` Nothing


