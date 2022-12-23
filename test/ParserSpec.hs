{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Code
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
    symbol `over` "#" `shouldBe` Right (Symbol '#')
    symbol `over` "1" `shouldBe` Right (Symbol '1')

  it "parses whole tapes" $ do
    toList <$> (tape `over` "(0 1 0 1)") `shouldBe` Right (map Symbol "0101")

  it "parses states" $ do
    state `over` "q0" `shouldBe` Right (State "q0")
    state `over` "1a" `shouldSatisfy` isLeft

  it "parses simulations (i.e. tapes)" $ do
    simulate `over` "(simulate machine (0 1 r #))" `shouldBe` Right (Simulation "machine" (fromList [Symbol '0', Symbol '1', Symbol 'r', Symbol '#']))
    simulate `over` "(simulate machine 0 1 r #)" `shouldSatisfy` isLeft
    simulate `over` "(simulate (0 1 r #))" `shouldSatisfy` isLeft
    simulate `over` "(simulate ([ ]))" `shouldSatisfy` isLeft

  it "parses rules" $ do
    rule `over` "(s  0 s  1 R)" `shouldBe` Right (State "s",  Symbol '0', State "s",  Symbol '1', R)
    rule `over` "(q0 # q1 . S)" `shouldBe` Right (State "q0", Symbol '#', State "q1", Blank,      S)
    rule `over` "(s 0 s 1 T)" `shouldSatisfy` isLeft
    rule `over` "(s 0 1 s R)" `shouldSatisfy` isLeft
    rule `over` "(0 0 0 0 S)" `shouldSatisfy` isLeft

  it "parses machine definitions" $ do
    let def = "(machine zeros-to-one\n\
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
                        [ (State "s", Symbol '0', State "s", Symbol '1', R)
                        , (State "s", Blank     , State "x", Blank     , L)
                        , (State "x", Symbol '1', State "x", Symbol '1', L)
                        , (State "x", Blank     , State "f", Blank     , R)
                        ])
                    (State "s")
                    Running
    definition `over` def `shouldBe` Right (Definition "zeros-to-one" expected)

  it "parses machine definitions regardless of the internal's order" $ do
    let def = "(machine zeros-to-one\n\
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
                        [ (State "s", Symbol '0', State "s", Symbol '1', R)
                        , (State "s", Blank     , State "x", Blank     , L)
                        , (State "x", Symbol '1', State "x", Symbol '1', L)
                        , (State "x", Blank     , State "f", Blank     , R)
                        ])
                    (State "s")
                    Running
    definition `over` def `shouldBe` Right (Definition "zeros-to-one" expected)

  it "doesn't parse wrong definitions" $ do
    let def = "(machine wrong\n\
              \  rules\n\
              \    ((s 0 s 1 R)\n\
              \     (s . x . L)\n\
              \     (x 1 x 1 L)\n\
              \     (x . f . R))\n\
              \ ; And this comment shall disappear\n\
              \  initial s\n\
              \)\n\
              \"
    definition `over` def `shouldSatisfy` isLeft

  it "parses an entire code file" $ do
    let contents1 = "\
                   \(machine  m1 initial s finals (q) rules ((s 0 s 1 R)))\n\
                   \(simulate m1 (0))\n\
                   \(machine  m2 initial s finals (q) rules ((s 0 s 1 R)))\n\
                   \(simulate m2 (1))\
                   \"
        contents2 = "; File description\n" <> contents1
        expectedMachine = TuringMachine
                     (State "s")
                     [State "q"]
                     (buildTransitions [(State "s", Symbol '0', State "s", Symbol '1', R)])
                    (State "s")
                    Running

    (code `over` contents1) `shouldBe` Right
                                        [ Definition "m1" expectedMachine
                                        , Simulation "m1" (fromList [Symbol '0'])
                                        , Definition "m2" expectedMachine
                                        , Simulation "m2" (fromList [Symbol '1'])
                                        ]
    (code `over` contents1) `shouldBe` (code `over` contents2)
