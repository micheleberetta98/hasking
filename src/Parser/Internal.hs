{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal where

import           Data.Char
import           Data.Text                  (Text)
import           Data.Void
import           Tape                       hiding (empty)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           TuringMachine.Internal

-----------------------------------------------
-- Types
-----------------------------------------------

type Parser = Parsec Void Text

data Code = Code (TuringMachine String String) [Tape String]

-----------------------------------------------
-- Interface
-----------------------------------------------

-- | Parses the whole code
parseCode :: Parser Code
parseCode = sc *> (Code <$> parseMachine <*> many parseSimulate)

-- | Parses the actual machine definition
parseMachine :: Parser (TuringMachine String String)
parseMachine = lexeme $ parens (symbol "machine" *> definition)
  where
    definition = mkMachine
      <$> lexeme initialState
      <*> lexeme finalStates
      <*> rules

    initialState = parens $ symbol "initial" *> parseState
    finalStates = parens $ symbol "finals" *> parens (many parseState)
    rules = buildTransitions <$> parens (symbol "rules" *> parens (many parseRule))

-- | Parses a single "rule" in the form @(state symbol state symbol direction)@
parseRule :: Parser (Rule String String)
parseRule = lexeme . parens $ (,,,,)
  <$> (parseState <* sc)
  <*> (parseSymbol <* sc)
  <*> (parseState <* sc)
  <*> (parseSymbol <* sc)
  <*> parseDirection

-- | Parses a @simulate-on@ definition
parseSimulate :: Parser (Tape String)
parseSimulate = lexeme $ parens (symbol "simulate-on" *> parseTape)

-- | Parses a state value
parseState :: Parser (State String)
parseState = State <$> lexeme identifier <?> "state"
  where identifier = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

-- | Parses a tape
parseTape :: Parser (Tape String)
parseTape = Tape.fromList <$> parens (many parseSymbol) <?> "tape (symbols' list)"

-- | Parses a symbol
parseSymbol :: Parser (Symbol String)
parseSymbol = lexeme (blank <|> symbolValue) <?> "symbol"
  where
    blank = Blank <$ string "."
    symbolValue = Symbol <$> some (noneOf [' ', '(', ')', '[', ']', '{', '}', ';'])

-- | Parses a @Direction@
parseDirection :: Parser Direction
parseDirection = choice
  [ R <$ symbol "R"
  , L <$ symbol "L"
  , S <$ symbol "S"
  ] <?> "direction"

-----------------------------------------------
-- Utilities
-----------------------------------------------

-- | Wrapper for symbols, picks up all trailing white space
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Wrapper for lexems, picks up all trailing white space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | A space consumer, ignores comments (beginning with @;@)
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

-- | Wraps the parser @p@ in parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(" ) (sc *> string ")")
