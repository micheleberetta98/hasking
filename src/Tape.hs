module Tape
  ( Tape
  , Direction(..)
  , Symbol(..)
  , fromList
  , toList
  , value
  , write
  , move
  ) where

-----------------------------------------------
-- Data declarations
-----------------------------------------------
-- | Represents a Symbol, which can be the control symbol `Blank` or user defined data
data Symbol a = Blank | Symbol a deriving (Show, Eq)

-- | An infinite Tape of Symbols of type `a`, which can be moved either *left* or *right*
data Tape a = Cell
    { value :: Symbol a
    , left  :: Tape a
    , right :: Tape a
    }
  deriving (Show)

-- | Represents movement (`L` = left, `R` = right, `S` = stay)
data Direction = L | R | S deriving (Show)

-----------------------------------------------
-- Tape conversions
-----------------------------------------------
-- | A tape consisting only of `Blank`
empty :: Tape a
empty = Cell Blank empty empty

-- | Converts a list into a `Tape`
fromList :: [a] -> Tape a
fromList = fromList' empty

fromList' :: Tape a -> [a] -> Tape a
fromList' l []     = Cell Blank l empty
fromList' l (x:xs) = h
  where h = Cell{ value = Symbol x, left = l, right = fromList' h xs }

-- | Converts a `Tape` into a list (it only traverses the tape right)
toList :: Tape a -> [a]
toList (Cell Blank _ _)         = []
toList (Cell (Symbol x) _ next) = x : toList next

-----------------------------------------------
-- Tape actions
-----------------------------------------------
-- | Move the `Tape` in a specified direction
move :: Direction -> Tape a -> Tape a
move S = id
move L = left
move R = right

-- | Writes a symbol to the `Tape`
write :: Symbol a -> Tape a -> Tape a
write symbol (Cell _ l r) = head
  where
    head = Cell symbol prev next
    prev = l{ right = head }
    next = r{ left = head }
