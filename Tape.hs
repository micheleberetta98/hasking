module Tape
  ( Tape
  , Movement(..)
  , Symbol(..)
  , fromList
  , toList
  , value
  , write
  , move
  ) where

----------------------------------------------- DATA
data Symbol a = Blank | Symbol a deriving (Show, Eq)

data Tape a = Cell
    { value :: Symbol a
    , left  :: Tape a
    , right :: Tape a
    }
  deriving (Show)

data Movement = L | R | S

----------------------------------------------- TAPE CONVERSIONS
empty :: Tape a
empty = Cell Blank empty empty

fromList :: [a] -> Tape a
fromList = fromList' empty

fromList' :: Tape a -> [a] -> Tape a
fromList' l []     = Cell Blank l empty
fromList' l (x:xs) = h
  where h = Cell{ value = Symbol x, left = l, right = fromList' h xs }

toList :: Tape a -> [a]
toList (Cell Blank _ _)         = []
toList (Cell (Symbol x) _ next) = x : toList next

----------------------------------------------- TAPE ACTIONS
move :: Movement -> Tape a -> Tape a
move S = id
move L = left
move R = right

write :: Symbol a -> Tape a -> Tape a
write symbol (Cell _ l r) = head
  where
    head = Cell symbol prev next
    prev = l{ right = head }
    next = r{ left = head }
