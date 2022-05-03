{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Data.Char
import           Data.Either
import           Parser.Internal
import           Pretty
import           Tape
import           Test.Hspec
import           Text.Megaparsec        hiding (State)
import           TuringMachine.Internal hiding (machine)

over :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
p `over` input = parse p "" input

parserTests :: SpecWith ()
parserTests = describe "Parser" $ do
  it "parses keywords" $ do
    keyword "$" `over` "$" `shouldBe` Right "$"
    keyword "$" `over` "$   " `shouldBe` Right "$"

  it "parses lexemes" $ do
    let number = read <$> many (satisfy isDigit)
    lexeme number `over` "1234" `shouldBe` Right 1234
    lexeme number `over` "77" `shouldBe` Right 77

  it "ignores comments" $ do
    keyword "c" `over` "c ; Comment" `shouldBe` Right "c"

  it "parses parenthesis" $ do
    let number = read <$> many (satisfy isDigit)
    parens number `over` "(1234)" `shouldBe` Right 1234
    parens number `over` "[1234]" `shouldSatisfy` isLeft

  it "parses directions" $ do
    direction `over` "R" `shouldBe` Right R
    direction `over` "L" `shouldBe` Right L
    direction `over` "S" `shouldBe` Right S
    direction `over` "T" `shouldSatisfy` isLeft

  it "parses tape symbols" $ do
    symbol `over` "." `shouldBe` Right Blank
    symbol `over` "[" `shouldSatisfy` isLeft
    symbol `over` "#" `shouldBe` Right (Symbol "#")
    symbol `over` "1" `shouldBe` Right (Symbol "1")

  it "parses whole tapes" $ do
    pretty <$> (tape `over` "(0 1 0 1)") `shouldBe` Right "0 1 0 1"

  it "parses states" $ do
    state `over` "q0" `shouldBe` Right (State "q0")
    state `over` "1a" `shouldSatisfy` isLeft

  it "parses simulations (i.e. tapes)" $ do
    pretty <$> (simulateOn `over` "(simulate-on (0 1 r #))") `shouldBe` Right "0 1 r #"
    simulateOn `over` "(simulate-on 0 1 r #)" `shouldSatisfy` isLeft
    simulateOn `over` "(simulateon (0 1 r #))" `shouldSatisfy` isLeft
    simulateOn `over` "(simulateon ([ ]))" `shouldSatisfy` isLeft

  it "parses rules" $ do
    rule `over` "(s  0 s  1 R)" `shouldBe` Right (State "s",  Symbol "0", State "s",  Symbol "1", R)
    rule `over` "(q0 # q1 . S)" `shouldBe` Right (State "q0", Symbol "#", State "q1", Blank,      S)
    rule `over` "(s 0 s 1 T)" `shouldSatisfy` isLeft
    rule `over` "(s 0 1 s R)" `shouldSatisfy` isLeft
    rule `over` "(0 0 0 0 S)" `shouldSatisfy` isLeft

  it "parses machine definitions" $ do
    let def = "(machine\n\
              \ ; Ignoring comments\n\
              \  initial s\n\
              \  finals (f)\n\
              \  rules\n\
              \    ((s 0 s 1 R)\n\
              \     (s . x . L)\n\
              \     (x 1 x 1 L)\n\
              \     (x . f . R)))\n\
              \"
        expected = TuringMachine
                     (State "s")
                     [State "f"]
                     ( buildTransitions
                        [ (State "s", Symbol "0", State "s", Symbol "1", R)
                        , (State "s", Blank     , State "x", Blank     , L)
                        , (State "x", Symbol "1", State "x", Symbol "1", L)
                        , (State "x", Blank     , State "f", Blank     , R)
                        ])
                      (State "s")
                      Running
    machine `over` def `shouldBe` Right expected

  it "parses machine definitions regardless of the internal's order" $ do
    let def = "(machine\n\
              \ ; Ignoring comments\n\
              \  finals (f)\n\
              \  rules\n\
              \    ((s 0 s 1 R)\n\
              \     (s . x . L)\n\
              \     (x 1 x 1 L)\n\
              \     (x . f . R))\n\
              \ ; And this comment shall disappear\n\
              \  initial s\n\
              \)\n\
              \"
        expected = TuringMachine
                     (State "s")
                     [State "f"]
                     ( buildTransitions
                        [ (State "s", Symbol "0", State "s", Symbol "1", R)
                        , (State "s", Blank     , State "x", Blank     , L)
                        , (State "x", Symbol "1", State "x", Symbol "1", L)
                        , (State "x", Blank     , State "f", Blank     , R)
                        ])
                      (State "s")
                      Running
    machine `over` def `shouldBe` Right expected

  it "doesn't parse wrong definitions" $ do
    let def = "(machine\n\
              \  rules\n\
              \    ((s 0 s 1 R)\n\
              \     (s . x . L)\n\
              \     (x 1 x 1 L)\n\
              \     (x . f . R))\n\
              \ ; And this comment shall disappear\n\
              \  initial s\n\
              \)\n\
              \"
    machine `over` def `shouldSatisfy` isLeft
