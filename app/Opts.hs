{-# LANGUAGE OverloadedStrings #-}

module Opts
  ( Options(..)
  , Command(..)
  , FileInput(..)
  , getOpts
  ) where

import qualified Data.Text           as T
import           Options.Applicative
import           Parser              (parseTape)
import           Tape                (Tape)

------------------------------------------------
-- Data types
------------------------------------------------

-- | The command line options
data Options
  = ShowVersion
  | Options
    { optCommand :: Command
    , optInput   :: FileInput
    }
  deriving (Show)

data Command
  = Run
  | Simulate
    { simMachineName :: String
    , simTape        :: Tape String
    }
  deriving (Show)

data FileInput = StdIn | File FilePath
  deriving (Show)

------------------------------------------------
-- Interface
------------------------------------------------

getOpts :: IO Options
getOpts = execParser opts
  where
    opts = info ((pVersion <|> pOpts) <**> helper)
           (  fullDesc
           <> progDesc "A Turing Machine Interpreter written in Haskell"
           <> header "HASKING")

------------------------------------------------
-- Options parsers
------------------------------------------------

pVersion :: Parser Options
pVersion = ShowVersion <$ flag' ShowVersion (long "version" <> short 'v' <> help "Show the version")

-- | Parses the options
pOpts :: Parser Options
pOpts = Options
  <$> pCommand
  <*> (   flag' StdIn (long "stdin" <> help "Read from standard input")
      <|> (File <$> argument str (metavar "INPUT" <> help "The input Hasking file")))

-- | Subcommand parser
pCommand :: Parser Command
pCommand = hsubparser
  (  command "run" (info (pure Run) (progDesc "Execute Hasking source code"))
  <> command "sim" (info pSimCommand (progDesc "Simulate the execution of a machine"))
  )

-- | The simulate command
pSimCommand :: Parser Command
pSimCommand = Simulate
  <$> strOption
      (  long "machine"
      <> short 'm'
      <> metavar "MACHINE"
      <> help "The machine to be simulated")
  <*> option tapeReader
      (  long "tape"
      <> short 't'
      <> metavar "TAPE"
      <> help "The tape to use for the simulation")
  where
    tapeReader = eitherReader (toEither . parseTape . T.pack)
    toEither (Right t) = Right t
    toEither _        = Left "Invalid tape provided, must be in the form (a b c ...)"

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
