module Main (main) where

import           Code          (MachineCode (MachineCode), parseCode)
import           Opts          (getOpts)
import           System.IO     (hPrint, hPutStrLn, stderr)
import           Tape          (Symbol, Tape, toList)
import           TuringMachine (State, machine)

main :: IO ()
main = do
  opts <- getOpts
  let (input, output) = opts
  input >>= runMachine >>= output

-- | Runs the machine with the specified code
runMachine :: String -> IO String
runMachine code = do
  case parseCode code of
    Left errors -> do
      hPrint stderr errors
      return ""

    Right (MachineCode ts start finish tape) -> do
      result (machine ts start finish tape)

-- | Formats the final tape in a nice way, or it prints the errors
result :: (Show s, Show a) => Either (State s, Symbol a) (Tape String) -> IO [Char]
result (Left (s, symbol)) = do
  hPutStrLn stderr $ "(?) invalid state found: (state = " ++ show s ++ ", symbol = " ++ show symbol ++ ")"
  return ""
result (Right t) = do
  return $ "{" ++ unwords (toList t) ++ "}"
