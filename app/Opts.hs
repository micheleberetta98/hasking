module Opts (Options, getOpts) where

import           System.Console.GetOpt (ArgDescr (NoArg, ReqArg),
                                        ArgOrder (RequireOrder), OptDescr (..),
                                        getOpt, usageInfo)

import           Data.List             (foldl')
import           Instruction           (Instruction (TapeValue),
                                        parseInstruction)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure, exitSuccess)
import           System.IO             (hPutStrLn, stderr)
import           Tape                  (Tape, fromList)

------------------------------------------------
-- Data types
------------------------------------------------

-- | The command line options
data Options = Options
  { input  :: IO String
  , output :: String -> IO ()
  , tape   :: Maybe (Tape String)
  }

------------------------------------------------
-- Options building
------------------------------------------------

-- | Parses the options from the cli
getOpts :: IO (IO String, String -> IO (), Maybe (Tape String))
getOpts = do
  args <- getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  opts <- foldl' (>>=) (return defaultOpts) actions
  return (input opts, output opts, tape opts)

------------------------------------------------
-- Options definition
------------------------------------------------

-- | The default options
defaultOpts :: Options
defaultOpts = Options { input = getContents, output = putStrLn, tape = Nothing }

-- | The options description
options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "i" ["input"]
        (ReqArg withInput "FILE")
        "The input file. If not specified, it will use user input (such as when piping)"
    , Option "o" ["output"]
        (ReqArg withOutput "FILE")
        "The output file. If not specified, it will use the standard output."
    , Option "t" ["tape"]
        (ReqArg withTape "TAPE")
        "The initial tape in the format {Symbol, Symbol, ...}. If not specified, it will be searched in the input file."
    , Option "v" ["version"]
        (NoArg printVersion)
        "Print the program version"
    , Option "h" ["help"]
        (NoArg help)
        "Show this help page"
    ]

-- | Sets the input option
withInput :: Monad m => FilePath -> Options -> m Options
withInput arg opt = return opt{ input = readFile arg }

-- | Sets the output option
withOutput :: Monad m => FilePath -> Options -> m Options
withOutput arg opt = return opt{ output = writeFile arg }

-- | Sets the initial tape option
withTape :: String -> Options -> IO Options
withTape arg opt =
  case parseInstruction arg of
    Right (TapeValue t) -> return opt{ tape = Just $ fromList t }
    _ -> do
      hPutStrLn stderr "Invalid tape provided"
      exitFailure


-- | Prints the version
printVersion :: a -> IO b
printVersion = const $ do
  hPutStrLn stderr "1.1.0"
  exitSuccess

-- | Prints the usage
help :: a -> IO b
help = const $ do
  hPutStrLn stderr $ usageInfo "Hasking - A Turing Machine Interpreter written in Haskell" options
  exitSuccess
