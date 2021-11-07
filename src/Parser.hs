module Parser (parseCode) where


import           Code               (Code (..), Definition (Definition),
                                     Rule (..), Simulate (..), State (State))
import           Data.Bifunctor     (Bifunctor (first))
import           Data.Char          (isAlpha, isAlphaNum, isSpace)
import           Data.Functor       (($>))
import           Tape               (Direction (L, R, S), Symbol (..), Tape,
                                     fromList)
import           Text.Parsec        hiding (State)
import           Text.Parsec.String (Parser)

-----------------------------------------------
-- Interface
-----------------------------------------------

-- | Parses the whole code
parseCode :: Parser Code
parseCode = Code
  <$> parseDefinition <* many space
  <*> spaced parseSimulate

-- | Parses the actual machine definition
parseDefinition :: Parser Definition
parseDefinition = parens $ string "machine" *> many1 space *> definition
  where
    definition = Definition
      <$> (initialState <* many space)
      <*> (finalStates <* many space)
      <*> rulesList

    initialState = parens $ string "initial" *> many1 space *> parseState
    finalStates = parens $ string "finals" *> many1 space *> parens (spaced parseState)

rulesList :: Parser [Rule]
rulesList = parens $ string "rules" *> many1 space *> parens (spaced parseRule)

-- | Parses a single "rule" in the form @(state symbol state symbol direction)@
parseRule :: Parser Rule
parseRule = parens $ Rule
  <$> (parseState <* many1 space)
  <*> (parseSymbol <* many1 space)
  <*> (parseState <* many1 space)
  <*> (parseSymbol <* many1 space)
  <*> parseDirection

-- | Parses a @simulate-on@ definition
parseSimulate :: Parser Simulate
parseSimulate = Simulate <$> parens (string "simulate-on" *> many1 space *> parseTape)

-- | Parses a state value
parseState :: Parser (State String)
parseState = State <$> identifier
  where
    identifier = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

-- | Parses a tape
parseTape :: Parser (Tape String)
parseTape = Tape.fromList <$> parens (spaced parseSymbol)

-- | Parses a symbol
parseSymbol :: Parser (Symbol String)
parseSymbol = blank <|> value
  where
    blank = string "." $> Blank
    value = Symbol <$> many1 (alphaNum <|> noneOf " ()[]{};")

-- | Parses a @Direction@
parseDirection :: Parser Direction
parseDirection = choice
  [ char 'R' $> R
  , char 'L' $> L
  , char 'S' $> S
  ]

-----------------------------------------------
-- Utilities
-----------------------------------------------

-- | Wraps the parser @p@ in parentheses
parens :: Parser a -> Parser a
parens = between (string "(" <* many space) (many space *> string ")")

-- | Parses zero or more instances of @p@ separated (end eventually ended) by space
spaced :: Parser a -> Parser [a]
spaced p = p `sepEndBy` many space

-- | Parses one or more instances of @p@ separated (end eventually ended) by space
spaced1 :: Parser a -> Parser [a]
spaced1 p = p `sepEndBy1` many space
