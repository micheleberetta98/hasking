{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal where

import           Data.Char
import           Data.Text                  (Text)
import           Data.Void
import           Tape                       (Direction, Symbol, Tape)
import qualified Tape                       as T
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           TuringMachine.Internal     (Rule, State (State), TuringMachine,
                                             buildTransitions, mkMachine)

-----------------------------------------------
-- Types
-----------------------------------------------

type Parser = Parsec Void Text

data Code = Code (TuringMachine String String) [Tape String]

-----------------------------------------------
-- Interface
-----------------------------------------------

-- | Parses the whole code
code :: Parser Code
code = sc *> (Code <$> machine <*> many simulateOn)

-- | Parses the actual machine definition
machine :: Parser (TuringMachine String String)
machine = lexeme $ def "machine" definition
  where
    definition = mkMachine
      <$> def "initial" state
      <*> defs "finals" state
      <*> (buildTransitions <$> defs "rules" rule)

-- | Parses a single "rule" in the form @(state symbol state symbol direction)@
rule :: Parser (Rule String String)
rule = lexeme . parens $ (,,,,)
  <$> state
  <*> symbol
  <*> state
  <*> symbol
  <*> direction

-- | Parses a @simulate-on@ definition
simulateOn :: Parser (Tape String)
simulateOn = def "simulate-on" tape

-- | Parses a state value
state :: Parser (State String)
state = State <$> lexeme identifier <?> "state"
  where identifier = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

-- | Parses a tape
tape :: Parser (Tape String)
tape = T.fromList <$> parens (many symbol) <?> "tape (list of symbols)"

-- | Parses a symbol
symbol :: Parser (Symbol String)
symbol = lexeme (blank <|> symbolValue) <?> "symbol"
  where
    blank = T.Blank <$ string "."
    symbolValue = T.Symbol <$> some (noneOf [' ', '(', ')', '[', ']', '{', '}', ';'])

-- | Parses a @Direction@
direction :: Parser Direction
direction = choice
  [ T.R <$ keyword "R"
  , T.L <$ keyword "L"
  , T.S <$ keyword "S"
  ] <?> "direction"

-----------------------------------------------
-- Utilities
-----------------------------------------------

-- | Little utility for a definition in the for @(keyword ...)@
def :: Text -> Parser a -> Parser a
def kw p = lexeme $ parens (keyword kw *> p)

-- | Little utility for a definition of a list of some parser, i.e. in the for @(keyword (...))@
defs :: Text -> Parser a -> Parser [a]
defs kw p = lexeme $ parens (keyword kw *> parens (many p))

-- | Wrapper for symbols, picks up all trailing white space
keyword :: Text -> Parser Text
keyword = L.symbol sc

-- | Wrapper for lexems, picks up all trailing white space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | A space consumer, ignores comments (beginning with @;@)
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

-- | Wraps the parser @p@ in parentheses
parens :: Parser a -> Parser a
parens = between (char '(' <* sc) (sc *> char ')')
