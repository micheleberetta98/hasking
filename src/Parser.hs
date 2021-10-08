module Parser
  ( Parser(runParser)
  , parseCode
  ) where

import           Control.Applicative (Alternative (empty, (<|>)),
                                      Applicative (liftA2))

import           Code                (Code (..), Definition (Definition),
                                      Rule (..), Simulate (..), State (State))
import           Data.Bifunctor      (Bifunctor (first))
import           Data.Char           (isAlpha, isAlphaNum, isSpace)
import           Data.Functor        (($>))
import           Tape                (Direction (L, R, S), Symbol (..), Tape,
                                      fromList)

-----------------------------------------------
-- Data declarations
-----------------------------------------------

-- | A @Parser@ is a function that, when executed on a string, returns
-- maybe a value @a@ followed by the rest of the string
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

------------------------------------------------
-- Instances
------------------------------------------------

instance Functor Parser where
  fmap f p = Parser $ fmap (first f) . runParser p

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  p1 <*> p2 = Parser p3
    where
      p3 s = runParser p1 s >>= runP2
      runP2 (f, xs') = runParser (f <$> p2) xs'

instance Alternative Parser where
  empty = Parser (const Nothing )
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2

------------------------------------------------
-- Parsing functions
------------------------------------------------

-- | Parses the whole code
parseCode :: Parser Code
parseCode = Code
  <$> parseDefinition <* many space
  <*> many (parseSimulate <* many space)
  <* many space

-- | Parses the actual machine definition
parseDefinition :: Parser Definition
parseDefinition = parens $ string "machine" *> many1 space *> (Definition <$> initialState <*> finalStates <*> rulesList)
  where
    initialState = parens (string "initial" *> many1 space *> parseState) <* many space
    finalStates = parens (string "finals" *> many1 space *> parens (sepEndBy1 space parseState)) <* many space
    rulesList = parens (string "rules" *> many1 space *> parens (sepEndBy1 space parseRule))

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
parseTape = Tape.fromList <$> parens (sepEndBy space parseSymbol)

-- | Parses a symbol
parseSymbol :: Parser (Symbol String)
parseSymbol = blank <|> (Symbol <$> value)
  where
    blank = char '.' $> Blank
    value = alphaNum <|> noneOf " ()[]{};"

-- | Parses a @Direction@
parseDirection :: Parser Direction
parseDirection =
    char 'R' $> R
    <|> char 'L' $> L
    <|> char 'S' $> S

------------------------------------------------
-- Utilities
------------------------------------------------

-- | Wraps the parser @p@ in parentheses
parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

-- | Parses @many@ of @p@ separated (and maybe ended) by @sep@
sepEndBy :: Parser a -> Parser b -> Parser [b]
sepEndBy sep p = many (p <* many1 sep) <* many sep

-- | Parses @many1@ of @p@ separated (and maybe ended) by @sep@
sepEndBy1 :: Parser a -> Parser b -> Parser [b]
sepEndBy1 sep p = many (p <* many1 sep) <* many sep

-- | Parses a single space
space :: Parser Char
space = satisfy isSpace

-- | Parser for a single char @c@
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Parses a series of alphanumberic characters
alphaNum :: Parser String
alphaNum = many1 (satisfy isAlphaNum)

-- | Parses one or more of any character that isn't in the specified ones
noneOf :: String -> Parser String
noneOf elems = many1 $ satisfy (`notElem` elems)

-- | Parses an exact string given as an argument
string :: String -> Parser String
string ""     = pure ""
string (c:cs) = (:) <$> char c <*> string cs

-- | Parses zero or more occurences given a parser @p@
many ::  Parser a -> Parser [a]
many p = many1 p <|> pure []

-- | Parses one or more occurences given a parser @p@
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- | Parses a single char that satisfies the predicate @p@
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing
