module Main (main) where

import           Code
import           Data.List
import           Data.Text       (Text, pack)
import           Data.Void
import           Opts            (Options (Options), getOpts)
import           Parser
import           Pretty
import           System.Exit
import           System.IO
import           Tape
import           Text.Megaparsec hiding (empty)
import           TuringMachine

main :: IO ()
main = do
  opts <- getOpts
  let (Options input output t _) = opts
  content <- input
  case getCode t content of
    Left errors -> do
      hPutStrLn stderr (errorBundlePretty errors)
      exitFailure
    Right code -> do
      output . pack . intercalate "\n" $ executeMachine code

executeMachine :: Code -> [String]
executeMachine code = map (format . machine tm) tapes
  where
    tm = fromCode code
    tapes = map getSimulationTape (simulations code)

    format (Left (state, symbol)) = concat ["(!) Invalid state reached: (", pretty state, ", ", pretty symbol, ")"]
    format (Right m) = pretty $ tape m

getCode :: Maybe (Tape String) -> Text -> Either (ParseErrorBundle Text Void) Code
getCode Nothing          c = parse parseCode "" c
getCode (Just extraTape) c = addSimulation extraTape <$> parse parseCode "" c
