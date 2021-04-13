module Tape
  ( Tape
  , Direction(..)
  , Symbol(..)
  , fromList
  , toList
  , value
  , write
  , move
  , empty
  ) where

import           Pretty (Pretty (..), prettyList, wrap)

-----------------------------------------------
-- Data declarations
-----------------------------------------------
-- | Represents a Symbol, which can be the control symbol `Blank` or user defined data
data Symbol a = Blank | Symbol a deriving (Show, Eq)

-- | An infinite Tape of Symbols of type `a`, which can be moved either *left* or *right*
data Tape a = Cell { value :: Symbol a, left :: Tape a, right :: Tape a }
  deriving (Show, Eq)

-- | Represents movement (`L` = left, `R` = right, `S` = stay)
data Direction = L | R | S deriving (Show, Eq)

-----------------------------------------------
-- Tape conversions
-----------------------------------------------

-- | A tape consisting only of `Blank`
empty :: Tape a
empty = Cell Blank empty empty

-- | Converts a list into a `Tape`
fromList :: [Symbol a] -> Tape a
fromList = fromList' empty

fromList' :: Tape a -> [Symbol a] -> Tape a
fromList' l []     = Cell Blank l empty
fromList' l (x:xs) = h
  where h = Cell{ value = x, left = l, right = fromList' h xs }

-- | Converts a `Tape` into a list (it only traverses the tape to the right)
toList :: Tape a -> [Symbol a]
toList (Cell Blank _ _) = []
toList (Cell x _ next)  = x : toList next

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
write symbol (Cell _ l r) = h
  where
    h = Cell symbol prev next
    prev = updateRight h l
    next = updateLeft h r

-- | Updates the left reference (recursively) in a tape
updateLeft :: Tape a -> Tape a -> Tape a
updateLeft to (Cell s _ r) = h
  where
    h = Cell s to next
    next = updateLeft h r

-- | Updates the right reference (recursively) in a tape
updateRight :: Tape a -> Tape a -> Tape a
updateRight to (Cell s l _) = h
  where
    h = Cell s prev to
    prev = updateRight h l

-----------------------------------------------
-- Instances
-----------------------------------------------

instance (Pretty s) => Pretty (Symbol s) where
  pretty (Symbol s) = pretty s
  pretty Blank      = "."

instance (Ord s) => Ord (Symbol s) where
  compare Blank Blank             = EQ
  compare Blank _                 = LT
  compare _ Blank                 = GT
  compare (Symbol s1) (Symbol s2) = compare s1 s2

instance (Pretty a) => Pretty (Tape a) where
  pretty t = wrap "{" (prettyList $ toList t) "}"

instance Pretty Direction where
  pretty = show
