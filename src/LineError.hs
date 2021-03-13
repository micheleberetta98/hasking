module LineError
  ( LineError
  , ErrorList
  , empty
  , justOne
  , simpleError
  , linedError
  , line
  , errorMsg
  , (.+)
  , (.++)
  ) where

import           Data.List

------------------------------------------------
-- Data types
------------------------------------------------

-- | An error message with a line (not required) associated
newtype LineError = LineError (Maybe Int, String)

-- | Represents a list of error messages with a line (not required) associated
newtype ErrorList = ErrorList [LineError]

------------------------------------------------
-- Utils
------------------------------------------------

-- | An empty error list
empty :: ErrorList
empty = ErrorList []

-- | Creates a list with a single error
justOne :: LineError -> ErrorList
justOne le = ErrorList [le]

-- | Creates a simple error without a line
simpleError :: String -> LineError
simpleError msg = LineError (Nothing, msg)

-- | Creates an error with a line
linedError :: String -> Int -> LineError
linedError msg l = LineError (Just l, msg)

-- | Retrieves the line of a `LineError`
line :: LineError -> Maybe Int
line (LineError (l, _)) = l

-- | Retrieves the message of a `LineError`
errorMsg :: LineError -> String
errorMsg (LineError (_, s)) = s

-- | Adds an error to the error list
(.+) :: ErrorList -> LineError -> ErrorList
es .+ le = es .++ justOne le

-- | Combines two ErrorList together
(.++) :: ErrorList -> ErrorList -> ErrorList
ErrorList es1 .++  ErrorList es2 = ErrorList (es1 ++ es2)

------------------------------------------------
-- Instances
------------------------------------------------

instance Eq LineError where
  LineError a == LineError b = a == b

instance Show LineError where
  show (LineError (Nothing, s)) = "(!) " ++ s
  show (LineError (Just l, s))  = "(!) at line " ++ show l ++ ": " ++ s

instance Eq ErrorList where
  ErrorList a == ErrorList b = a == b

instance Show ErrorList where
  show (ErrorList []) = ""
  show (ErrorList es) = intercalate "\n" . map show $ sortOn line es
