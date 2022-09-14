{-# LANGUAGE OverloadedStrings #-}

module Opts
  ( Options(..)
  , Command(..)
  , FileInput(..)
  , getOpts
  ) where

import qualified Data.Text                as T
import           Options.Applicative
import           Options.Applicative.Help hiding (fullDesc)
import           Parser                   (parseTape)
import           Tape                     (Tape)
import qualified Tape

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
getOpts = execParser $
  info
    ((pVersion <|> pOpts) <**> helper)
    (fullDesc <> headerDoc title)

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
      <> value Tape.empty
      <> showDefault
      <> help "The tape to use for the simulation")
  where
    tapeReader = eitherReader (toEither . parseTape . T.pack)
    toEither (Right t) = Right t
    toEither _        = Left "Invalid tape provided, must be in the form (a b c ...)"

title :: Maybe Doc
title = Just $ string $ unlines
  [ "        __ __         __    _          "
  , "       / // /__  ___ / /__ (_)__  ___ _"
  , "      / _  / _ `(_-</  '_// / _ \\/ _ `/"
  , "     /_//_/\\_,_/___/_/\\_\\/_/_//_/\\_, / "
  , "                                /___/  "
  , ""
  , " A Turing Machine Interpreter written in Haskell"
  ]
