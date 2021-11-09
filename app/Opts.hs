{-# LANGUAGE OverloadedStrings #-}

module Opts (Options(Options), getOpts) where

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
        "The file containing the machine specification (default: stdin)"
    , Option "o" ["output"]
        (ReqArg withOutput "FILE")
        "The output file (default: stdout)"
    , Option "t" ["tape"]
        (ReqArg withTape "TAPE")
        "The initial tape in the format \"Symbol Symbol ...\""
    , Option "i" ["interactive"]
        (NoArg withInteractiveModeOn)
        "Run in interactive mode"
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
  hPutStrLn stderr "3.1.0"
  exitSuccess

-- | Prints the usage
help :: a -> IO b
help _ = do
    hPutStrLn stderr $ usageInfo title options
    exitSuccess

title :: String
title = unlines
  [ "        __ __         __    _          "
  , "       / // /__  ___ / /__ (_)__  ___ _"
  , "      / _  / _ `(_-</  '_// / _ \\/ _ `/"
  , "     /_//_/\\_,_/___/_/\\_\\/_/_//_/\\_, / "
  , "                                /___/  "
  , ""
  , " A Turing Machine Interpreter written in Haskell"
  ]
