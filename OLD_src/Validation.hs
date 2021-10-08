module Validation where

------------------------------------------------
-- Data types
------------------------------------------------

-- | The @Validation e a@ type behaves just like @Either@
-- but it accumulates @Err@s when used as an applicative,
-- but can't be a @Monad@ because @(>>=)@ and @(<*>)@ would disagree
-- (@e@ must be a @Semigroup@)
data Validation e a = Ok a | Err e
  deriving (Show, Eq)

------------------------------------------------
-- Instances
------------------------------------------------

instance Functor (Validation e) where
  fmap _ (Err e) = Err e
  fmap f (Ok a)  = Ok (f a)

instance (Semigroup e) => Applicative (Validation e) where
  pure = Ok
  Err a <*> Err b = Err (a <> b)
  Err a <*> _     = Err a
  Ok _  <*> Err b = Err b
  Ok f  <*> Ok a  = Ok (f a)

instance (Semigroup e) => Monad (Validation e) where
  Err e >>= _ = Err e
  Ok a  >>= f = f a
