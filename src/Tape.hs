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

import           Pretty (Pretty (..))

-----------------------------------------------
-- Types
-----------------------------------------------

-- | Represents a Symbol, which can be the control symbol @Blank@ or user defined data
data Symbol a = Blank | Symbol a
  deriving (Show, Eq)

-- | An infinite Tape of Symbols of type @a@, which can be moved either *left* or *right*
data Tape a = Cell
  { value :: Symbol a
  , left  :: Tape a
  , right :: Tape a }
  deriving (Show, Eq)

-- | Represents movement (@L@ = left, @R@ = right, @S@ = stay)
data Direction = L | R | S
  deriving (Show, Eq)

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
  pretty = pretty . toList

instance Pretty Direction where
  pretty = show


-----------------------------------------------
-- Tape conversions
-----------------------------------------------

-- | A tape consisting only of @Blank@
empty :: Tape a
empty = Cell Blank empty empty

-- | Converts a list into a @Tape@
fromList :: [Symbol a] -> Tape a
fromList xs = h
  where h = fromList' (updateRight h empty) xs

fromList' :: Tape a -> [Symbol a] -> Tape a
fromList' l []     = write Blank l
fromList' l (x:xs) = h
  where h = Cell{ value = x, left = updateRight h l, right = fromList' h xs }

-- | Converts a @Tape@ into a list (it only traverses the tape to the right)
toList :: Tape a -> [Symbol a]
toList (Cell Blank _ _) = []
toList (Cell x _ next)  = x : toList next

-- | Returns a fixed number of symbols in a tape, in particular
-- the @n@ symbols on the left and the @n@ symbols on the right
-- from the current position
toFixedList :: (Pretty a) => Int -> Tape a -> [Symbol a]
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
move :: Direction -> Tape a -> Tape a
move S = id
move L = left
move R = right

-- | Writes a symbol to the @Tape@
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
