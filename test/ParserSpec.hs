{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Code
import           Data.Char
import           Data.Either
import           Data.Text       (Text)
import           Parser.Internal
import           Pretty          (Pretty (pretty))
import           Tape
import           Test.Hspec
import           Text.Megaparsec hiding (State)

over :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
p `over` input = parse p "" input

parserTests :: SpecWith ()
parserTests = describe "Parser" $ do
  it "parses symbols" $ do
    symbol "$" `over` "$" `shouldBe` Right "$"
    symbol "$" `over` "$   " `shouldBe` Right "$"

  it "parses lexemes" $ do
    let number = read <$> many (satisfy isDigit)
    lexeme number `over` "1234" `shouldBe` Right 1234
    lexeme number `over` "77" `shouldBe` Right 77

  it "ignores comments" $ do
    symbol "c" `over` "c ; Comment" `shouldBe` Right "c"

  it "parses parenthesis" $ do
    let number = read <$> many (satisfy isDigit)
    parens number `over` "(1234)" `shouldBe` Right 1234
    parens number `over` "[1234]" `shouldSatisfy` isLeft

  it "parses directions" $ do
    parseDirection `over` "R" `shouldBe` Right R
    parseDirection `over` "L" `shouldBe` Right L
    parseDirection `over` "S" `shouldBe` Right S
    parseDirection `over` "T" `shouldSatisfy` isLeft

  it "parses tape symbols" $ do
    parseSymbol `over` "." `shouldBe` Right Blank
    parseSymbol `over` "[" `shouldSatisfy` isLeft
    parseSymbol `over` "#" `shouldBe` Right (Symbol "#")
    parseSymbol `over` "1" `shouldBe` Right (Symbol "1")

  it "parses whole tapes" $ do
    pretty <$> (parseTape `over` "0 1 0 1") `shouldBe` Right "0 1 0 1"
