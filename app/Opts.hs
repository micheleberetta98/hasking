{-# LANGUAGE OverloadedStrings #-}

module Opts (Options(..), getOpts) where

import           System.Console.GetOpt (ArgDescr (NoArg, ReqArg),
                                        ArgOrder (RequireOrder), OptDescr (..),
                                        getOpt, usageInfo)

import           Data.List
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Parser
import           System.Environment
import           System.Exit
import           System.IO
import           Tape
import           Text.Megaparsec

------------------------------------------------
-- Data types
------------------------------------------------

-- | The command line options
data Options = Options
  { input       :: IO Text
  , output      :: Text -> IO ()
  , tape        :: Maybe (Tape String)
  , interactive :: Bool
  }

------------------------------------------------
-- Options building
------------------------------------------------

-- | Parses the options from the cli
getOpts :: IO Options
getOpts = do
  args <- getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  foldl' (>>=) (return defaultOpts) actions

------------------------------------------------
-- Options definition
------------------------------------------------

-- | The default options
defaultOpts :: Options
defaultOpts = Options
  { input = TIO.getContents
  , output = TIO.putStrLn
  , tape = Nothing
  , interactive = False
  }

-- | The options description
options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "s" ["script"]
        (ReqArg withInput "FILE")
        "The file containing the machine specification. If not specified, it will use standard input (such as when piping)"
    , Option "o" ["output"]
        (ReqArg withOutput "FILE")
        "The output file. If not specified, it will use the standard output."
    , Option "t" ["tape"]
        (ReqArg withTape "TAPE")
        "The initial tape in the format {Symbol, Symbol, ...}, or even without the brackets.\nIt will overwrite any tape in the input file.\nIf not specified, it will be searched in the input file."
    , Option "i" ["interactive"]
        (NoArg withInteractiveModeOn)
        "Run in interactive mode (requires a script file and a tape specified)."
    , Option "v" ["version"]
        (NoArg printVersion)
        "Print the program version"
    , Option "h" ["help"]
        (NoArg help)
        "Show this help page"
    ]

-- | Sets the input option
withInput :: Monad m => FilePath -> Options -> m Options
withInput arg opts = return opts{ input = TIO.readFile arg }

-- | Sets the output option
withOutput :: Monad m => FilePath -> Options -> m Options
withOutput arg opts = return opts{ output = TIO.writeFile arg }

-- | Sets the initial tape option
withTape :: String -> Options -> IO Options
withTape arg opts =
  case parse parseTape "" formattedArg of
    Right t -> return opts{ tape = Just t }
    _ -> do
      hPutStrLn stderr "Invalid tape provided"
      exitFailure
  where
    formattedArg = T.concat ["(", T.pack arg, ")"]

withInteractiveModeOn :: Monad m => Options -> m Options
withInteractiveModeOn opts = return opts{ interactive = True }

-- | Prints the version
printVersion :: a -> IO b
printVersion = const $ do
  hPutStrLn stderr "2.1.0"
  exitSuccess

-- | Prints the usage
help :: a -> IO b
help = const $ do
  hPutStrLn stderr $ usageInfo "🖥  Hasking - A Turing Machine Interpreter written in Haskell" options
  exitSuccess
