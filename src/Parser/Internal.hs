{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal where

import           Code
import           Control.Applicative.Permutations
import           Data.Char
import           Data.Text                        (Text)
import           Data.Void
import qualified Tape                             as T
import           Tape                             (Direction, Symbol, Tape)
import           Text.Megaparsec                  hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L
import           TuringMachine.Internal           (Rule, State (State),
                                                   TuringMachine,
                                                   buildTransitions, mkMachine)

-----------------------------------------------
-- Types
-----------------------------------------------

type Parser = Parsec Void Text

-----------------------------------------------
-- Interface
-----------------------------------------------

-- | Parses the whole code
code :: Parser Code
code = do
  sc
  lexeme $ many $ choice
    [ try definition <?> "machine definition"
    , simulate <?> "machine simulation"
    ]

-- | Parses the actual machine definition
definition :: Parser Expression
definition = def "machine" (Definition <$> machineName <*> machine)
  where
    machine = runPermutation $
      mkMachine
        <$> toPermutation (def' "initial" state)
        <*> toPermutation (defs' "finals" state)
        <*> toPermutation (buildTransitions <$> defs' "rules" rule)

-- | Parses a single "rule" in the form @(state symbol state symbol direction)@
rule :: Parser (Rule String)
rule = lexeme . parens $ (,,,,) <$> state <*> symbol <*> state <*> symbol <*> direction

-- | Parses a simulation of a machine on a tape
simulate :: Parser Expression
simulate = def "simulate" (Simulation <$> machineName <*> tape)

-- | Parses a state value
state :: Parser (State String)
state = State <$> lexeme identifier <?> "state"
  where identifier = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

-- | Parses a tape
tape :: Parser Tape
tape = T.fromList <$> parens (many symbol) <?> "tape (list of symbols)"

-- | Parses a symbol
symbol :: Parser Symbol
symbol = lexeme (blank <|> symbolValue) <?> "symbol"
  where
    blank = T.Blank <$ string "."
    symbolValue = T.Symbol <$> noneOf [' ', '(', ')', '[', ']', '{', '}', ';']

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

machineName :: Parser String
machineName = lexeme $ some (noneOf [' ', '\n', '\t'])

-- | Little utility for a definition in the for @(keyword ...)@
def :: Text -> Parser a -> Parser a
def kw p = lexeme $ parens (def' kw p)

-- | Little utility for a definition of a list of some parser, i.e. in the for @(keyword (...))@
defs :: Text -> Parser a -> Parser [a]
defs kw p = lexeme $ parens (defs' kw p)

-- | Like 'def' but without the parentheses
def' :: Text -> Parser a -> Parser a
def' kw p = lexeme (keyword kw *> p)

-- | Like 'defs' but without the parentheses
defs' :: Text -> Parser a -> Parser [a]
defs' kw p = def' kw (parens (many p))

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
