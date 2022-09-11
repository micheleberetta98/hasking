module Main (main) where

import           Control.Monad   (void)
import           Data.List       (intercalate)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Void       (Void)
import           Opts            (Options (Options), getOpts)
import           Parser          (Code (..), parseCode)
import           Pretty          (Pretty (pretty))
import           System.Exit     (exitFailure)
import           System.IO       (hPutStrLn, stderr)
import           Tape            (Tape)
import           Text.Megaparsec (ParseErrorBundle, errorBundlePretty, parse)
import           TuringMachine   (TuringMachine, machine)
import           UI              (runUiWith)

type TM = TuringMachine String String

main :: IO ()
main = do
  opts <- getOpts
  let (Options input output t interactive) = opts
      load = addTape t <$> loadMachine input

  pure ()
  -- Code m tapes <- load

  -- if interactive
  --   then void $ executeUI m tapes load
  --   else output $ executeMachine m tapes

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
    format (Right (_, t))         = pretty t

addTape = undefined
-- addTape :: Maybe (Tape String) -> Code -> Code
-- addTape Nothing c                       = c
-- addTape (Just extraTape) (Code m tapes) = Code m (extraTape : tapes)

handleErrors :: Either (ParseErrorBundle Text Void) Code -> IO Code
handleErrors (Left errors) = hPutStrLn stderr (errorBundlePretty errors) >> exitFailure
handleErrors (Right c)     = pure c
