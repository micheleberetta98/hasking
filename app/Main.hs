{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Code
import           Data.List
import           Data.Text       (Text)
import qualified Data.Text.IO    as T
import           Data.Void
import           Opts
import           Parser
import           Pretty
import           System.Exit
import           System.IO
import           Text.Megaparsec
-- import           UI

main :: IO ()
main = getOpts >>= \case
  ShowVersion       -> hPutStrLn stderr "3.2.0" >> exitSuccess
  Options Run input -> do
    code <- readInput input >>= handleErrors . parseCode
    putStrLn $ intercalate "\n" $ map pretty $ execute code
  _ -> pure ()

readInput :: FileInput -> IO Text
readInput StdIn       = T.getContents
readInput (File path) = T.readFile path

handleErrors :: Either (ParseErrorBundle Text Void) Code -> IO Code
handleErrors (Left errors) = hPutStrLn stderr (errorBundlePretty errors) >> exitFailure
handleErrors (Right c)     = pure c
