module Parser
  ( Parser
  , parseCode
  , parseTape
  ) where

import           Code            (Code)
import           Data.Text       (Text)
import           Data.Void       (Void)
import           Parser.Internal
import           Tape            (Tape)
import           Text.Megaparsec

parseCode :: Text -> Either (ParseErrorBundle Text Data.Void.Void) Code
parseCode = parse code ""

parseTape :: Text -> Either (ParseErrorBundle Text Void) (Tape String)
parseTape = parse tape ""
