module Parser
  ( Parser(runParser)
  , alpha
  , alphaNum
  , char
  , identifier
  , noneOf
  , spaced
  , spaces
  , string
  , zeroOrMore
  ) where

import           Control.Applicative (Alternative (empty, (<|>)),
                                      Applicative (liftA2))

import           Data.Bifunctor      (Bifunctor (first))
import           Data.Char           (isAlpha, isAlphaNum, isSpace)

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
-- Functions
------------------------------------------------

-- | Parses an exact string given as an argument
string :: String -> Parser String
string ""     = pure ""
string (c:cs) = (:) <$> char c <*> string cs

------------------------------------------------
-- Utilities
------------------------------------------------

-- | Parses a single char that satisfies the predicate @p@
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing

-- | Parser for a single char @c@
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Parses a series of zero or more spaces
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- | Parses a series of alphabetic characters
alpha :: Parser String
alpha = oneOrMore (satisfy isAlpha)

-- | Parses a series of alphanumberic characters
alphaNum :: Parser String
alphaNum = oneOrMore (satisfy isAlphaNum)

-- | Parses one or more of any character that isn't in the specified ones
noneOf :: String -> Parser String
noneOf elems = oneOrMore $ satisfy (`notElem` elems)

-- | Parses an identifier, which is given by a letter followed by zero or more
-- letters or numbers
identifier :: Parser String
identifier = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

-- | Utility to transform a parser @p@ into a parser of the same type, but with
-- spaces (or no spaces) around
spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces

-- | Parses zero or more occurences given a parser @p@
zeroOrMore ::  Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- | Parses one or more occurences given a parser @p@
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

