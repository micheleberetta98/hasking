module Parser
  ( Parser(runParser)
  , char
  , integer
  , spaces
  , spaces1
  , astring
  , identifier
  , atom
  , spaced
  , spaced1
  , zeroOrMore
  , oneOrMore
  ) where

import           Control.Applicative (Alternative (empty, (<|>)),
                                      Applicative (liftA2))

import           Data.Bifunctor      (Bifunctor (first))
import           Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace)

-----------------------------------------------
-- Data declarations
-----------------------------------------------

-- | A `Parser` is a function that, when executed on a string, returns
-- maybe a value `a` followed by the rest of the string
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

------------------------------------------------
-- Parser functions and utilities
------------------------------------------------

-- | Parses a single char that satisfies the predicate `p`
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing

-- | Parser for a single char `c`
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Parser for an integer
integer :: Parser Int
integer = Parser f
  where
    f [] = Nothing
    f xs =
      case ns of
        "" -> Nothing
        ns -> Just (read ns, rest)
      where (ns, rest) = span isDigit xs

-- | Parses a series of zero or more spaces
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- | Parses a series of one or more spaces
spaces1 :: Parser String
spaces1 = oneOrMore (satisfy isSpace)

-- | Parses a series of alphabetic characters
astring :: Parser String
astring = oneOrMore (satisfy isAlpha)

-- | Parses an identifier, which is given by a letter followed by zero or more
-- letters or numbers
identifier :: Parser String
identifier = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

-- | Parses an atom, i.e. a string made of alphanumeric characters
atom :: Parser String
atom = oneOrMore (satisfy isAlphaNum)

-- | Utility to transform a parser `p` into a parser of the same type, but with
-- spaces (or no spaces) around
spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces

-- | Utility to transform a parser `p` into a parser of the same type, but with
-- spaces (or no spaces) before and at least 1 space after
spaced1 :: Parser a -> Parser a
spaced1 p = spaces *> p <* spaces1

-- ------------------------------------------------
-- -- Repetitions
-- ------------------------------------------------

-- | Parses zero or more occurences given a parser `p`
zeroOrMore ::  Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- | Parses one or more occurences given a parser `p`
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

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
