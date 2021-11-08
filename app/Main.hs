module Main (main) where

import           Code
import           Control.Monad
import           Data.List
import           Data.Maybe
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
  let
    (Options input output t interactive) = opts
    load = fmap (addTape t) <$> loadMachine input

  (m, tapes) <- load

  if interactive
    then void $ executeUI m tapes load
    else output (executeMachine m tapes)

loadMachine :: IO Text -> IO (TM, [Tape String])
loadMachine input = do
  code <- input >>= handleErrors . parse parseCode ""
  pure (fromCode code)

executeUI :: TM -> [Tape String] -> IO (TM, [Tape String]) -> IO ()
executeUI _ [] _ = do
  hPutStrLn stderr "Please specify a tape manually when running in interactive mode"
  void exitFailure
executeUI m (t:_) load = void $ runUiWith m t (fmap listToMaybe <$> load)

executeMachine :: TM -> [Tape String] -> Text
executeMachine tm = T.pack . intercalate "\n" . map (format . machine tm)
  where
    format (Left (state, symbol)) = concat ["(!) Invalid state reached: (", pretty state, ", ", pretty symbol, ")"]
    format (Right m) = pretty $ tape m

addTape :: Maybe (Tape String) -> [Tape String] -> [Tape String]
addTape Nothing          = id
addTape (Just extraTape) = (extraTape :)

handleErrors :: Either (ParseErrorBundle Text Void) Code -> IO Code
handleErrors (Left errors) = do
  hPutStrLn stderr (errorBundlePretty errors)
  exitFailure
handleErrors (Right m) = pure m
