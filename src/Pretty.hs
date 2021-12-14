{-# LANGUAGE FlexibleInstances #-}

module Pretty
  ( Pretty(..)
  , wrap
  , prettyList
  ) where

import           Data.List (intercalate)

-- | A typeclass to pretty-print stuff
-- I made this because show string = "string" and I don't like those double quotes
class Pretty a where
  pretty :: a -> String

instance Pretty String where
  pretty = id

instance Pretty Int where
  pretty = show

-- | Wraps a @Pretty@ instance content in two delimiters
wrap :: (Pretty a) => String -> a -> String -> String
wrap c1 content c2 = c1 ++ pretty content ++ c2

-- | Prettify all elements in a list and joins them with a space
prettyList :: (Pretty a) => [a] -> String
prettyList = unwords . map pretty
