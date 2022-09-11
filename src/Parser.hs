module Parser
  ( Parser
  , Code
  , Expression(..)
  , parseCode
  , parseTape
  ) where

import           Parser.Internal
import           Tape            (Tape)

parseCode :: Parser Code
parseCode = code

parseTape :: Parser (Tape String)
parseTape = tape
