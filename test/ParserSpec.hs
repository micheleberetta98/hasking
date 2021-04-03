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

  it "can parse alphanumeric strings" $ do
    let
      ok = runParser alphaNum "123 rest"
      notok = runParser alphaNum "**rest123"
    ok `shouldBe` Just ("123", " rest")
    notok `shouldBe` Nothing

  it "should parse spaces and spaced things" $ do
    runParser spaces "" `shouldBe` Just ("", "")
    runParser spaces "   x" `shouldBe` Just ("   ", "x")
    runParser spaces "x" `shouldBe` Just("", "x")

    runParser (spaced alphaNum) "  12  " `shouldBe` Just ("12", "")
    runParser (spaced alphaNum) "12  " `shouldBe` Just ("12", "")
    runParser (spaced alphaNum) "  12" `shouldBe` Just ("12", "")
    runParser (spaced alphaNum) "12" `shouldBe` Just ("12", "")
    runParser (spaced alphaNum) "  -x12  " `shouldBe` Nothing

  it "should parse pure strings" $ do
    runParser alpha "BEGIN" `shouldBe` Just ("BEGIN", "")
    runParser alpha "BEGIN123" `shouldBe` Just ("BEGIN", "123")
    runParser alpha "123" `shouldBe` Nothing

  it "should parse identifiers" $ do
    runParser identifier "stateName123,xyz" `shouldBe` Just ("stateName123", ",xyz")
    runParser identifier "123state,xyz" `shouldBe` Nothing

  it "should combine neatly" $ do
    runParser (identifier <|> alpha) "12465" `shouldBe` Nothing

    runParser (spaces *> identifier <* spaces) "   k123   " `shouldBe` Just ("k123", "")
    runParser (spaces *> identifier <* spaces) "k123" `shouldBe` Just ("k123", "")
    runParser (spaces *> identifier <* spaces) "  456  " `shouldBe` Nothing
    runParser (spaces *> identifier <* spaces) "" `shouldBe` Nothing


