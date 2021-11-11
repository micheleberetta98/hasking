{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal where

import           Code
import           Data.Char
import           Data.Text                  (Text)
import           Data.Void
import           Tape                       hiding (empty)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-----------------------------------------------
-- Types
-----------------------------------------------

type Parser = Parsec Void Text

-----------------------------------------------
-- Interface
-----------------------------------------------

-- | Parses the whole code
parseCode :: Parser Code
parseCode = sc *> (mkCode <$> lexeme parseDefinition <*> many' parseSimulate)

-- | Parses the actual machine definition
parseDefinition :: Parser Definition
parseDefinition = parens $ symbol "machine" *> definition
  where
    definition = mkDefinition
      <$> lexeme initialState
      <*> lexeme finalStates
      <*> rulesList

    initialState = parens $ symbol "initial" *> parseState
    finalStates = parens $ symbol "finals" *> parens (many' parseState)
    rulesList = parens $ symbol "rules" *> parens (many' parseRule)

-- | Parses a single "rule" in the form @(state symbol state symbol direction)@
parseRule :: Parser Rule
parseRule = parens $ mkRule
  <$> (parseState <* sc)
  <*> (parseSymbol <* sc)
  <*> (parseState <* sc)
  <*> (parseSymbol <* sc)
  <*> parseDirection

-- | Parses a @simulate-on@ definition
parseSimulate :: Parser Simulation
parseSimulate = mkSimulation <$> parens (symbol "simulate-on" *> parseTape)

-- | Parses a state value
parseState :: Parser State
parseState = mkState <$> identifier <?> "state"
  where
    identifier = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

-- | Parses a tape
parseTape :: Parser (Tape String)
parseTape = Tape.fromList <$> parens (many' parseSymbol) <?> "tape (symbols' list)"

-- | Parses a symbol
parseSymbol :: Parser (Symbol String)
parseSymbol = (blank <|> symbolValue) <?> "symbol"
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

-- | Parses @many@ of a certain @lexeme@
many' :: Parser a -> Parser [a]
many' = many . lexeme

-- | Wraps the parser @p@ in parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(" ) (sc *> string ")")
