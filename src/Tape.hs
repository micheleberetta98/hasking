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

-- | Represents a Symbol, which can be the control symbol @Blank@ or user defined data
data Symbol = Blank | Symbol Char
  deriving (Show, Eq)

-- | An infinite Tape of Symbols of type @a@, which can be moved either *left* or *right*
data Tape = Cell
  { value :: Symbol
  , left  :: Tape
  , right :: Tape
  }

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
  pretty t = "(" <> pretty (toList t) <> ")"

instance Pretty Direction where
  pretty = show

instance Show Tape where
  show = pretty

instance Eq Tape where
  (==) = (==) `on` toList

-----------------------------------------------
-- Tape conversions
-----------------------------------------------

-- | A tape consisting only of @Blank@
empty :: Tape
empty = Cell Blank empty empty

-- | Converts a list into a @Tape@
fromList :: [Symbol] -> Tape
fromList xs = h
  where h = fromList' (updateRight h empty) xs

fromList' :: Tape -> [Symbol] -> Tape
fromList' l []     = write Blank l
fromList' l (x:xs) = h
  where h = Cell{ value = x, left = updateRight h l, right = fromList' h xs }

-- | Converts a @Tape@ into a list (it only traverses the tape to the right)
toList :: Tape -> [Symbol]
toList (Cell Blank _ _) = []
toList (Cell x _ next)  = x : toList next

-- | Returns a fixed number of symbols in a tape, in particular
-- the @n@ symbols on the left and the @n@ symbols on the right
-- from the current position
toFixedList :: Int -> Tape -> [Symbol]
toFixedList n tape
  | n < 0     = []
  | n == 0    = [value tape]
  | otherwise = leftList ++ [value tape] ++ rightList
  where
    leftList = reverse $ toFixedList' n left tape
    rightList = toFixedList' n right tape

    toFixedList' 0 _ _ = []
    toFixedList' m k t =
      let t' = k t
      in value t' : toFixedList' (m-1) k t'

-----------------------------------------------
-- Tape actions
-----------------------------------------------

-- | Move the @Tape@ in a specified direction
move :: Direction -> Tape -> Tape
move S = id
move L = left
move R = right

-- | Writes a symbol to the @Tape@
write :: Symbol -> Tape -> Tape
write symbol (Cell _ l r) = h
  where
    h = Cell symbol prev next
    prev = updateRight h l
    next = updateLeft h r

-- | Updates the left reference (recursively) in a tape
updateLeft :: Tape -> Tape -> Tape
updateLeft to (Cell s _ r) = h
  where
    h = Cell s to next
    next = updateLeft h r

-- | Updates the right reference (recursively) in a tape
updateRight :: Tape -> Tape -> Tape
updateRight to (Cell s l _) = h
  where
    h = Cell s prev to
    prev = updateRight h l
