module Main (main) where

import           Code
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

main :: IO ()
main = do
  opts <- getOpts
  let (Options input output t interactive) = opts
  m <- addTape t <$> (input >>= handleErrors . parse parseCode "")

  if interactive
    then executeUI t m
    else output (executeMachine m)

executeUI :: Maybe (Tape String) -> Code -> IO ()
executeUI Nothing = const $ do
  hPutStrLn stderr "Please specify a tape manually when running in interactive mode"
  void exitFailure
executeUI (Just t) = void . runUiWith . withTape t . fromCode

executeMachine :: Code -> Text
executeMachine code = T.pack . intercalate "\n" . map (format . machine tm . getSimulationTape) . simulations $ code
  where
    tm = fromCode code
    format (Left (state, symbol)) = concat ["(!) Invalid state reached: (", pretty state, ", ", pretty symbol, ")"]
    format (Right m) = pretty $ tape m

addTape :: Maybe (Tape String) -> Code -> Code
addTape Nothing          = id
addTape (Just extraTape) = addSimulation extraTape

handleErrors :: Either (ParseErrorBundle Text Void) Code -> IO Code
handleErrors (Left errors) = do
  hPutStrLn stderr (errorBundlePretty errors)
  exitFailure
handleErrors (Right m) = pure m
