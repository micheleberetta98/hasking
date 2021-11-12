{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Code.Internal
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Either
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Parser.Internal
import           Pretty                 (Pretty (pretty))
import           Tape
import           Test.Hspec
import           Text.Megaparsec        hiding (State)

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
    pretty <$> (parseTape `over` "(0 1 0 1)") `shouldBe` Right "0 1 0 1"

  it "parses states" $ do
    parseState `over` "q0" `shouldBe` Right (State "q0")
    parseState `over` "1a" `shouldSatisfy` isLeft

  it "parses simulate (i.e. tapes)" $ do
    pretty <$> (parseSimulate `over` "(simulate-on (0 1 r #))") `shouldBe` Right "(simulate-on (0 1 r #))"
    parseSimulate `over` "(simulate-on 0 1 r #)" `shouldSatisfy` isLeft
    parseSimulate `over` "(simulateon (0 1 r #))" `shouldSatisfy` isLeft
    parseSimulate `over` "(simulateon ([ ]))" `shouldSatisfy` isLeft

  it "parses rules" $ do
    parseRule `over` "(s  0 s  1 R)" `shouldBe` Right (Rule (State "s")  (Symbol "0") (State "s") (Symbol "1") R)
    parseRule `over` "(q0 # q1 . S)" `shouldBe` Right (Rule (State "q0") (Symbol "#") (State "q1") Blank       S)
    parseRule `over` "(s 0 s 1 T)" `shouldSatisfy` isLeft
    parseRule `over` "(s 0 1 s R)" `shouldSatisfy` isLeft
    parseRule `over` "(0 0 0 0 S)" `shouldSatisfy` isLeft

  it "parses machine definitions" $ do
    let def = "(machine\n\
              \ ; Ignoring comments\n\
              \  (initial s)\n\
              \  (finals (f))\n\
              \  (rules\n\
              \    ((s 0 s 1 R)\n\
              \     (s . x . L)\n\
              \     (x 1 x 1 L)\n\
              \     (x . f . R))))\n\
              \"
    parseDefinition `over` def `shouldBe` Right (
      Definition (State "s") [State "f"]
        [ Rule (State "s") (Symbol "0") (State "s") (Symbol "1") R
        , Rule (State "s") Blank        (State "x") Blank        L
        , Rule (State "x") (Symbol "1") (State "x") (Symbol "1") L
        , Rule (State "x") Blank        (State "f") Blank        R
        ])
