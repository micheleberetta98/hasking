module Parser
  ( Parser(runParser)
  , char
  , integer
  , direction
  , spaces
  , spaces'
  , alphaString
  , identifier
  , zeroOrMore
  , oneOrMore
  ) where

import           Control.Applicative (Alternative (empty, (<|>)),
                                      Applicative (liftA2))
import           Data.Bifunctor      (first)
import           Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace)
import           Tape                (Direction (..))

-----------------------------------------------
-- Data declarations
-----------------------------------------------

-- | A `Parser` is a function that, when executed on a string, returns
-- maybe a value `a` followed by the rest of the string
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

------------------------------------------------
-- Parser functions
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

-- | Parser for a particular tape direction
direction :: Parser Direction
direction = Parser f
  where
    f []       = Nothing
    f ('L':xs) = Just (L, xs)
    f ('R':xs) = Just (R, xs)
    f ('S':xs) = Just (S, xs)
    f _        = Nothing

    -- | Parses a series of zero or more spaces
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- | Parses a series of one or more spaces
spaces' :: Parser String
spaces' = oneOrMore (satisfy isSpace)

-- | Parses a series of alphabetic characters
alphaString :: Parser String
alphaString = oneOrMore (satisfy isAlpha)

-- | Parses an identifier, which is given by a letter followed by zero or more
-- letters or numbers
identifier :: Parser String
identifier = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

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
  Parser p1 <*> p2 = Parser p3
    where
      p3 s = p1 s >>= runP2
      runP2 (f, xs') = runParser (f <$> p2) xs'

instance Alternative Parser where
  empty = Parser (const Nothing )
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2
