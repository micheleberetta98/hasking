module Main (main) where

import           Code          (MachineCode (MachineCode), fromCode)
import           Opts          (getOpts)
import           Pretty        (Pretty (..), prettyList, wrap)
import           System.Exit   (exitFailure)
import           System.IO     (hPrint, hPutStrLn, stderr)
import           Tape          (Symbol, Tape, toList)
import           TuringMachine (State, machine)

main :: IO ()
main = do
  opts <- getOpts
  let (input, output, tape) = opts
  input >>= runMachine . addTape tape >>= output

-- | Runs the machine with the specified code
runMachine :: String -> IO String
runMachine code = do
  case fromCode code of
    Left errors -> do
      hPutStrLn stderr $ pretty errors
      exitFailure

    Right (MachineCode ts start finish tape) -> do
      result (machine ts start finish tape)

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
