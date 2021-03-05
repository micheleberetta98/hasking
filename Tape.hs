module Tape
  ( Tape
  , Movement(..)
  , Symbol(..)
  , fromList
  , toList
  , get
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

data Movement = MLeft | MRight | MStay

----------------------------------------------- TAPE CONVERSIONS
empty :: Tape a
empty = Cell Blank empty empty

fromList :: [a] -> Tape a
fromList = fromList' empty

fromList' :: Tape a -> [a] -> Tape a
fromList' prev []     = Cell Blank prev empty
fromList' prev (x:xs) = h
  where
    h = Cell (Symbol x) prev next
    next = fromList' h xs

toList :: Tape a -> [a]
toList (Cell Blank _ _)         = []
toList (Cell (Symbol x) _ next) = x : toList next

----------------------------------------------- TAPE ACTIONS
move :: Movement -> Tape a -> Tape a
move MStay  tape             = tape
move MLeft  (Cell _ left _)  = left
move MRight (Cell _ _ right) = right

write :: Symbol a -> Tape a -> Tape a
write symbol (Cell _ l r) = head
  where
    head = Cell symbol prev next
    prev = l{ right = head }
    next = r{ left = head }

get :: Tape a -> Symbol a
get = value
