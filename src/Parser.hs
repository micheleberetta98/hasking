module Parser
  ( Parser
  , parseCode
  , parseTape
  ) where

import           Code            (Code)
import           Parser.Internal
import           Tape            (Tape)

parseCode :: Parser Code
parseCode = code

parseTape :: Parser (Tape String)
parseTape = tape
