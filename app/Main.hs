module Main where

import           Code
import           Control.Monad
import           System.Directory
import           System.Environment
import           System.IO
import           Tape
import           TuringMachine

main :: IO ()
main = do
  fname <- getArgs >>= validateArgs
  unless (null fname) $ do
    readFile fname >>= runMachine

validateArgs :: [String] -> IO FilePath
validateArgs []        = putStrLn "Missing filename (first argument)" >> return ""
validateArgs (fname:_) = do
  ok <- doesFileExist fname
  if ok
    then return fname
    else putStrLn ("No such file: `" ++ fname ++ "`") >> return ""

runMachine :: String -> IO ()
runMachine code = do
  case parseCode code of
    Left errors                               -> print errors
    Right (MachineCode ts start finish tape) -> do
      let result = machine ts start finish tape
      putStrLn (prettyTape result)

prettyTape :: (Show s, Show a) => Either (State s, Symbol a) (Tape String) -> String
prettyTape (Left (s, symbol))  = "? invalid state found: (state = " ++ show s ++ ", symbol = " ++ show symbol ++ ")"
prettyTape (Right t) = "OK\nOutput tape: {" ++ unwords (toList t) ++ "}"
