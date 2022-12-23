module Tape
  ( Tape
  , Direction(..)
  , Symbol(..)
  , empty
  , fromList
  , move
  , toFixedList
  , toList
  , value
  , write
  ) where

import           Data.Function (on)
import           Pretty        (Pretty (..))

-----------------------------------------------
-- Types
-----------------------------------------------


-- | An infinite Tape of Symbols of type @a@, which can be moved either *left* or *right*
data Tape = Tape
  { prev  :: Infinite Symbol
  , value :: Symbol
  , next  :: Infinite Symbol
  }

data Infinite a = a :| Infinite a

-- | Represents a Symbol, which can be the control symbol @Blank@ or user defined data
data Symbol = Blank | Symbol Char
  deriving (Show, Eq)

-- | Represents movement (@L@ = left, @R@ = right, @S@ = stay)
data Direction = L | R | S
  deriving (Show, Eq)

-----------------------------------------------
-- Instances
-----------------------------------------------

instance Pretty Symbol where
  pretty (Symbol s) = [s]
  pretty Blank      = "."

instance Ord Symbol where
  compare Blank Blank             = EQ
  compare Blank _                 = LT
  compare _ Blank                 = GT
  compare (Symbol s1) (Symbol s2) = compare s1 s2

instance Pretty Tape where
  pretty t = "(" <> pretty (toFixedList 5 t) <> ")"

instance Pretty Direction where
  pretty = show

-- This instances have no actual meaning
-- they should be used only for testing

instance Show Tape where
  show = show . toList

instance Eq Tape where
  (==) = (==) `on` toFixedList 10

-----------------------------------------------
-- Tape conversions
-----------------------------------------------

-- | A tape consisting only of @Blank@
empty :: Tape
empty = Tape (infinitely Blank) Blank (infinitely Blank)

-- | Converts a list into a @Tape@
fromList :: [Symbol] -> Tape
fromList []     = empty
fromList (x:xs) = Tape (infinitely Blank) x (listToInfinite Blank xs)

-- | Converts a @Tape@ into a list (it only traverses the tape to the right)
toList :: Tape -> [Symbol]
toList (Tape _ Blank _) = []
toList (Tape _ x xs)    = x : takeWhile (/= Blank) (infiniteToList xs)

-- | Returns a fixed number of symbols in a tape, in particular
-- the @n@ symbols on the left and the @n@ symbols on the right
-- from the current position
toFixedList :: Int -> Tape -> [Symbol]
toFixedList n (Tape l x r)
  | n < 0     = []
  | n == 0    = [x]
  | otherwise = leftList ++ [x] ++ rightList
  where
    leftList = reverse $ take n $ infiniteToList l
    rightList = take n $ infiniteToList r

-----------------------------------------------
-- Tape actions
-----------------------------------------------

-- | Move the @Tape@ in a specified direction
move :: Direction -> Tape -> Tape
move S t                     = t
move L (Tape (l :| ls) x rs) = Tape ls l (x :| rs)
move R (Tape ls x (r :| rs)) = Tape (x :| ls) r rs

-- | Writes a symbol to the @Tape@
write :: Symbol -> Tape -> Tape
write symbol (Tape l _ r) = Tape l symbol r


-----------------------------------------------
-- Infinite utils
-----------------------------------------------

infinitely :: a -> Infinite a
infinitely x = x :| infinitely x

listToInfinite :: a -> [a] -> Infinite a
listToInfinite def = foldr (:|) (infinitely def)

infiniteToList :: Infinite a -> [a]
infiniteToList (x :| xs) = x : infiniteToList xs
