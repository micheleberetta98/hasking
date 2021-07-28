module Main (main) where

import           Code          (MachineCode (MachineCode), fromCode)
import           Control.Monad (void)
import           Opts          (Options (Options), getOpts)
import           Pretty        (Pretty (..))
import           System.Exit   (exitFailure)
import           System.IO     (hPutStrLn, stderr)
import           Tape          (Symbol, Tape)
import           TuringMachine (State, machine)
import           UI            (runUiWith)
import           Validation    (Validation (Err, Ok))

main :: IO ()
main = do
  opts <- getOpts
  let (Options input output tape interactive) = opts
  m <- input >>= getMachine . addTape tape
  if interactive
    then void (runUiWith m)
    else runMachine m >>= output

getMachine :: String -> IO MachineCode
getMachine code =
  case fromCode code of
    Err errors -> do
      hPutStrLn stderr $ pretty errors
      exitFailure

    Ok m -> return m

-- | Runs the machine with the specified code
runMachine :: MachineCode -> IO String
runMachine (MachineCode ts start finish tape) = result (machine ts start finish tape)

-- | Adds the tape to the provided code
addTape :: Maybe (Tape String) -> String -> String
addTape Nothing s  = s
addTape (Just t) s = s ++ "\n" ++ pretty t

-- | Formats the final tape in a nice way, or it prints the errors
result :: (Pretty s, Pretty a, Pretty t) => Either (State s, Symbol a) (Tape t) -> IO [Char]
result (Left (s, symbol)) = do
  hPutStrLn stderr $ "(?) invalid state found: (state = " ++ pretty s ++ ", symbol = " ++ pretty symbol ++ ")"
  exitFailure
result (Right t) = do
  return $ pretty t
