module Main (main) where

import           Control.Monad
import           Data.List
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Void
import           Opts
import           Parser
import           Pretty
import           System.Exit
import           System.IO
import           Tape
import           Text.Megaparsec hiding (empty)
import           TuringMachine
import           UI

type TM = TuringMachine String String

main :: IO ()
main = do
  opts <- getOpts
  let (Options input output t interactive) = opts
      load = addTape t <$> loadMachine input

  Code m tapes <- load

  if interactive
    then void $ executeUI m tapes load
    else output (executeMachine m tapes)

loadMachine :: IO Text -> IO Code
loadMachine input = input >>= handleErrors . parse parseCode ""

executeUI :: TM -> [Tape String] -> IO Code -> IO ()
executeUI _ [] _ = do
  hPutStrLn stderr "Please specify a tape manually when running in interactive mode"
  void exitFailure
executeUI m (t:_) load = void $ runUiWith m t load

executeMachine :: TM -> [Tape String] -> Text
executeMachine tm = T.pack . intercalate "\n" . map (format . machine tm)
  where
    format (Left (state, symbol)) = concat ["(!) Invalid state reached: (", pretty state, ", ", pretty symbol, ")"]
    format (Right (_, t)) = pretty t

addTape :: Maybe (Tape String) -> Code -> Code
addTape Nothing c                       = c
addTape (Just extraTape) (Code m tapes) = Code m (extraTape : tapes)

handleErrors :: Either (ParseErrorBundle Text Void) Code -> IO Code
handleErrors (Left errors) = do
  hPutStrLn stderr (errorBundlePretty errors)
  exitFailure
handleErrors (Right c) = pure c
