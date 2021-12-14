{-# LANGUAGE FlexibleInstances #-}

module Pretty where

-- | A typeclass to pretty-print stuff
-- I made this because show string = "string" and I don't like those double quotes
class Pretty a where
  pretty :: a -> String

instance {-# OVERlAPS #-} Pretty String where
  pretty = id

instance {-# OVERlAPS #-} Pretty a => Pretty [a] where
  pretty = unwords . map pretty

instance Pretty Int where
  pretty = show
